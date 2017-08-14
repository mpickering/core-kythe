{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
module KythePlugin(plugin) where

import Data.Text.Prettyprint.Doc
--import Data.Text.Prettyprint.Doc.Render.Util.StackMachine
import Data.Text.Prettyprint.Doc.Render.Util.SimpleDocTree
import Control.Monad.Writer hiding ((<>))
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens hiding ((<.>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.IO as T
import PprCore
import Unique (getKey, mkUniqueGrimily)
import qualified FastString as FS
import Outputable (SDoc(..))
import OutputableAnnotation
import GhcPlugins (Plugin(..), defaultPlugin, CoreM, CoreToDo(..), CommandLineOption, ModGuts(..)
                  , getDynFlags, defaultUserStyle, initSDocContext, Module
                  , NamedThing(..), Name, isValName, RealSrcLoc)
import PrelNames ( wildCardName )
import Name
import UniqFM (nonDetUFMToList, listToUFM_Directly, plusUFM )
import Control.Arrow (first)
import qualified SrcLoc as GHC
import Module hiding (getModule)
import qualified GhcPlugins as GHC ( getModule, Expr(..), Tickish(..) )
import Language.Haskell.Indexer.Translate hiding (Pos(..))
import qualified Language.Haskell.Indexer.Translate as P (Pos(..))
import CoreSyn hiding (Expr (..))
import System.FilePath
import NameEnv
import Data.Maybe
import Debug.Trace
import Control.Monad.Identity (runIdentity)
import Control.Monad.Morph (lift, hoist)
--import qualified Data.ByteString as B
import Data.Conduit (($$), (=$=), awaitForever,  Sink)

import qualified Language.Kythe.Schema.Raw as Raw
import qualified Language.Kythe.Schema.Raw.Proto as Raw
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as Builder

import Language.Haskell.Indexer.Frontend.Kythe
import Data.Conduit.List (chunksOf)
import Data.ProtoLens ( encodeMessage )
import Data.Bits
import Control.Applicative
import qualified Data.Map as M

-- DataTypes

data Pos = Pos { _x :: Int, _y :: Int } deriving (Eq, Ord, Show)

--instance Ord Name where
--  compare n1 n2 = getKey (nameUnique n1) `compare` getKey (nameUnique n2)

data OutputState = OutputState { _pos :: Pos
                               , _topNameMap :: NameEnv Tick
                               , _localNameMap :: M.Map (Name, SrcSpan) Tick -- Span is the span of the enclosing bind
                               , _binderMap :: NameEnv Span
                               , _sourceNoteMap :: M.Map SrcSpan (GHC.RealSrcSpan, String)
                               }

data Scope = TopLevel | Local

data OutputReader = OutputReader { _outFile :: FilePath
                                 , _curModule :: Module
                                 , _curDeclSpan :: SrcSpan }

data SrcSpan = SS Pos Pos FilePath deriving (Show, Eq)

instance Ord SrcSpan where
  (SS p1 p2 fp) `compare` (SS p3 p4 fp')
    = if p1 <= p3 && p2 >= p4 then LT else GT


-- The intermediate type we use
data XRefs = XRefs { _outDecls :: [Decl]  -- Decls
                   , _outReferences :: (M.Map (Name, SrcSpan) Tick -> NameEnv Tick
                                                                  -> [TickReference])
                   -- Delayed references, the nameenv
                   -- is a map from decl name to their ticks
                   , _outRelations :: M.Map Span Tick -> [Relation] } -- Generates edges


makeLenses ''OutputState
makeLenses ''OutputReader
makeLenses ''Pos
makeLenses ''XRefs

cvtSrcSpan :: SrcSpan -> Span
cvtSrcSpan (SS (Pos x1 y1) (Pos x2 y2) fp) =
  let cfp = makeSourcePath fp
  in Span (P.Pos y1 x1 cfp) (P.Pos y2 x2 cfp)

cvtGhcSrcSpan :: GHC.SrcSpan -> Maybe Span
cvtGhcSrcSpan ss = case ss of
    GHC.UnhelpfulSpan _ -> Nothing
    GHC.RealSrcSpan r -> Just $ Span (realToPos . GHC.realSrcSpanStart $ r)
                                 (realToPos . GHC.realSrcSpanEnd $ r)
  where
    realToPos :: RealSrcLoc -> P.Pos
    realToPos r =
        let path = SourcePath . T.pack . FS.unpackFS . GHC.srcLocFile $ r
        in P.Pos (GHC.srcLocLine r) (GHC.srcLocCol r) path

-- The plugin

plugin :: Plugin
plugin = defaultPlugin {
  installCoreToDos = printCore
}

printCore :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
printCore clos ctd = do
  let outputDir = case clos of
                [] -> error "Must set output directory"
                (outdir:_) -> outdir
  return (ctd ++ [printPass outputDir])

printPass :: FilePath -> CoreToDo
printPass fp = CoreDoPluginPass "Print Pass" (doPrint fp)

doPrint :: FilePath -> ModGuts -> CoreM ModGuts
doPrint outdir mgs@ModGuts{mg_binds, mg_module, mg_rdr_env} = do
    let outpath = makeOutPath outdir mg_module
        entriesPath = makeEntriesPath outdir mg_module
    --liftIO $ putStrLn outpath
    dflags <- getDynFlags
    curm <- GHC.getModule
    let ctx = initSDocContext dflags (defaultUserStyle dflags)
        sdoc = runSDoc (pprCoreBindingsWithAnn mg_binds)
    --liftIO $ putStrLn "Running printer"
    let ((pprint, st), xrefs) = runRender outpath curm (render (sdoc ctx))
    --liftIO $ T.putStrLn pprint
    --liftIO $ putStrLn $ showXrefs xrefs (view nameMap st)
    liftIO $ T.writeFile outpath pprint
    --liftIO $ print (view sourceNoteMap st) mg_rdr_env
    --let rels = processSourceNotes (view sourceNoteMap st)
    let xref = toXRef mg_module outpath xrefs (view topNameMap st)
                                              (view localNameMap st)

    liftIO $ output outpath entriesPath xref
    return mgs

{-
processSourceNotes :: M.Map SrcSpan (GHC.RealSrcSpan, String)
                   -> GlobalRdrEnv
                   -> [Relation]
processSourceNotes
-}

lenientDecodeUtf8 :: FilePath -> IO T.Text
lenientDecodeUtf8 = fmap (T.decodeUtf8With T.lenientDecode) . B.readFile

makeOutPath, makeEntriesPath :: FilePath -> Module -> FilePath
makeOutPath = makeModulePath "core"
makeEntriesPath = makeModulePath "entries"

makeModulePath :: String -> FilePath -> Module -> FilePath
makeModulePath suf fp md = fp </> modName <.> suf
  where
    modName = moduleNameString (moduleName md)

-- TODO set the second argument properly
makeAnalysedFile :: FilePath -> AnalysedFile
makeAnalysedFile (makeSourcePath -> outpath) = AnalysedFile outpath outpath

makeModuleTick :: Module -> ModuleTick
makeModuleTick hmod = ModuleTick
                      { mtPkgModule = makePkgModule hmod
                      , mtSpan = Nothing }

makePkgModule :: Module -> PkgModule
makePkgModule (Module uid modname)
  = traceShowId $ PkgModule { getPackage = T.pack (munge $ FS.unpackFS (unitIdFS uid))
              , getModule =  T.pack (moduleNameString modname)  }
  where
    munge "main" = "main_main"
    munge t = t

makeSourcePath :: FilePath -> SourcePath
makeSourcePath fp = SourcePath (T.pack fp)


writeDecl :: Decl -> XRefs
writeDecl d = writeDecls [d]

writeDecls :: [Decl] -> XRefs
writeDecls d = set outDecls d mempty

type DelayedTickReference = M.Map (Name, SrcSpan) Tick -> NameEnv Tick -> [TickReference]


writeReference :: DelayedTickReference -> XRefs
writeReference dt  = set outReferences dt mempty

writeRelation :: (M.Map Span Tick  -> Relation) -> XRefs
writeRelation r = set outRelations (\m -> [r m]) mempty

toXRef :: Module -> FilePath -> XRefs -> NameEnv Tick -> M.Map (Name, SrcSpan) Tick -> XRef
toXRef hmod fp (XRefs ds nets rs)  ne le=
    XRef { xrefFile      = makeAnalysedFile fp
    , xrefModule    = makeModuleTick hmod
    , xrefDecls     = ds
    , xrefCrossRefs = nets le ne
    , xrefRelations = rs (traceShowId (toSSMap ne))
    , xrefImports   = []
    }

instance Monoid XRefs where
  mempty = XRefs [] (\ _ _ -> []) (\_ -> [])
  (XRefs ds trs rs) `mappend` (XRefs ds' trs' rs') =
    XRefs (ds ++ ds') (\d e -> trs d e ++ trs' d e) (\d -> rs d ++ rs' d)

type Output a = ReaderT OutputReader (StateT OutputState (WriterT XRefs Identity)) a

render :: Doc PExpr -> Output T.Text
render ast = go (treeForm (layoutPretty customLayoutOptions ast))
  where
    customLayoutOptions =
      defaultLayoutOptions
        { layoutPageWidth = AvailablePerLine 150 1 }

    go tf =
      case tf of
        STEmpty ->
          pure mempty
        STChar c -> do
          pos . x %= (+1)
          pure (T.singleton c)
        STText l t -> do
          pos . x %= (+ l)
          return t
        STLine i -> do
          pos . y %= (+1)
          pos . x .= (i + 1) -- 1-indexing
          return (T.singleton '\n' <> T.replicate i " ")
        -- The control flow here is quite difficult.
        -- 1) We need to print out the thing recursively in order to get
        -- the whole span of the thing we are going to annotate
        -- 2) BUT we need to know the ticks of things in the outer scope
        -- in order to generate references for things in the inner scope.
        --
        -- The first attempt here made whatCore return a function
        -- NameEnv Tick -> TickReferences which collected all the decls and
        -- then resolved the references but this doesn't work for locally
        -- scoped things which causes reference clashes.
        STAnn ann rest -> mdo
          let k m = case ann of
                    PBind {} ->
                      local (\s -> set curDeclSpan ss s) m
                    _ -> m
          (ss, res) <- k (withSS (go rest))
          whatCore ann ss >>= tell
          return res


        STConcat xs -> fmap mconcat (traverse go xs)


{-
NOTE: SourceNotes

We want to generate edges from core bindings to what generated them.
We can do this with SourceNotes, we can't express too much granularity as
we need to link together nodes. So for now we just use the information in the
first source note to link a whole definition to one place in the source file.

Our strategy is to not directly generate the edges at first. We want to later
iterate over GlobalRdrElt and pick possible definition sites for each of the things we
find in there. We do this by consulting the name of everything in GlobalRdrElt and finding
SrcSpans which *start* at the same place as one of the references.

We then have enough information to emit an edge.

So when first using whatCore, we just write the first SourceNote into a map
from ContainingDecl -> SourceNote, we only keep the outermost span which is the last
one which is written. Once done, we invert the map into SourceNote -> [ContainingDecl]
and use the SrcLoc name to find which edges we need to emit.

-}

-- Returns the Source span of the result of outputing the thing
withSS :: Output a -> Output (SrcSpan, a)
withSS o = do
  p <- use pos
  res <- o
  p' <- use pos
  out <- view outFile
  return ((SS p p' out), res)

toSSMap :: NameEnv Tick -> M.Map Span Tick
toSSMap n = M.fromList $ catMaybes ([(,t) <$> tickSpan t | t <- (nameEnvElts n)])

whatCore :: PExpr -> SrcSpan -> Output XRefs
whatCore (PCoreExpr e) ss =
-- Need to particularlly deal with binders here
  case e of
    GHC.Lam b eb -> do
      let (bs, _e') = collectBinders eb
      writeDecls <$> mapMaybeM (goMakeDecl Local ss) (b:bs)
    GHC.Let b _e ->
      writeDecls <$> makeDecl Local b ss
    GHC.Case _ b _ as -> do
      let binders = concatMap (\(_, bs, _) -> bs) as
      writeDecls <$> mapMaybeM (goMakeDecl Local ss) (b:binders) -- Not sure what this b is fore
    GHC.Tick (GHC.SourceNote ss sn) e -> do
      enclosingSpan <- view curDeclSpan
      m <- view curModule
      -- 1. Use the enclosingSpan tick to find the actual tick
      -- This keeps the outermost sourceNote
      --sourceNoteMap %= (M.insert enclosingSpan (ss, sn))
      -- Make the end tick from the info we have (hopefully)
      let endTick = traceShowId $ Tick
            { tickSourcePath = makeSourcePath (FS.unpackFS $ GHC.srcSpanFile ss)
            , tickPkgModule = makePkgModule m
            , tickThing = (T.pack sn)
            -- If we pass in a SrcSpan, otherwise use a random one so that
            -- we can still emit references
            , tickSpan = cvtGhcSrcSpan (GHC.RealSrcSpan ss)
            , tickUniqueInModule = True
            , tickTermLevel = True
            }
      -- Use lookupLT here are the ordering is by inclusion.
      -- s1 < s2 iff s1 is enclosed by s2
      let lup env = case M.lookupGT (cvtSrcSpan enclosingSpan) env of
                      Nothing -> error (show env ++ show enclosingSpan)
                      Just v -> traceShow (fst v, enclosingSpan) (snd v)
      return $ writeRelation (\env -> Relation endTick (Generates "haskell") (lup env))
      --return mempty



    _ -> return mempty
{-
  case e of
    GHC.Var {} -> "Var" -- Does this cause double wrapping?
    GHC.Lit {} -> "Lit"
    GHC.App {} -> "App"
    GHC.Let {} -> "Let"
    GHC.Case {} -> "Case"
    GHC.Cast {} -> "Cast"
    GHC.Tick {} -> "Tick"
    GHC.Type {} -> "Type"
    GHC.Coercion {} -> "Co"
-}
whatCore (PBind b) ss = do
 writeDecls <$> makeDecl TopLevel b ss
whatCore (PVar b v) ss  =
  case b of
    Binder ->   do
      addBinder v ss -- We already print out binding sites as decls but need
      --traceM ("Adding binder info for: " ++ show (getOccString v) ++ show ss)
      return mempty
                                 -- to record exactly where the binder is
                                 -- for better highlighting
    Reference -> do
      outf <- view outFile
      cm <- view curModule
      enclosingSpan <- view curDeclSpan
      let rt localEnv n = maybeToList (makeReferenceTick outf cm (getName v) (cvtSrcSpan ss) enclosingSpan localEnv n)
      return $ writeReference rt -- References generate reference ticks

makeReferenceTick :: FilePath -> Module -> Name -> Span -> SrcSpan -> M.Map (Name, SrcSpan) Tick -> NameEnv Tick -> Maybe TickReference
makeReferenceTick outf cm n ss enclosingSpan localEnv nenv =
  case lookupNameEnv nenv n <|> M.lookup (n, enclosingSpan) localEnv of
    -- For debugging for now
    Nothing -> do
      -- Names without in-module definitions
      traceM ("Couldn't find name in nameenv: " ++ show (getOccString n) ++ show ss)
--      traceM (show $ nameEnvElts localEnv)
      let refTargetTick = nameInModuleToTick Nothing outf cm n
          refSourceSpan = ss
          refHighLevelContext = Nothing
          refKind = Ref
      return $ TickReference{..}

    Just t ->
      let refTargetTick = t
          refSourceSpan = ss
          refHighLevelContext = Nothing -- Need to set this by setting the name ctxt in reader state
          refKind = Ref -- This needs additional information from PprCore, I think it's possible to add
      in do

        --when (getOccString n == "t")
        --  (traceM ("Adding reference tick for: " ++ show (getOccString n) ++ show t))
        Just $ TickReference{..}


makeDecl :: forall b . (NamedThing b) => Scope -> Bind b -> SrcSpan -> Output [Decl]
makeDecl sc bi  ss =
  case bi of
    NonRec b _eb -> maybeToList <$> goMakeDecl sc ss b
    Rec bs -> mapMaybeM (goMakeDecl sc ss . fst) bs

goMakeDecl :: NamedThing b => Scope -> SrcSpan -> b -> Output (Maybe Decl)
goMakeDecl topLevel ss b =
      let
          declType = StringyType "" ""
          declExtra = Nothing
          name = getName b
      in do
        declIdentifierSpan <- uses binderMap (\n -> lookupNameEnv n (name))
        case declIdentifierSpan of
          Nothing -> do
            traceShowM ((nameOccurenceText $ name), declIdentifierSpan)
            return Nothing
          Just ss -> do
            declTick <- makeDeclTick (name) ss
            case topLevel of
              TopLevel -> do
                traceM ("Adding to top name map" ++ getOccString name)
                topNameMap %= (\n -> extendNameEnv n (name) declTick)
              Local -> do
                traceM ("Adding to local name map: " ++ getOccString name)
                enclosingSpan <- view curDeclSpan
                localNameMap %= (M.insert (name, enclosingSpan) declTick)
            return (Just $ Decl{..})


nameInModuleToTick :: Maybe Span -> FilePath -> Module -> Name -> Tick
nameInModuleToTick ss sourcePath cm n =
    Tick
      { tickSourcePath = makeSourcePath sourcePath
      , tickPkgModule = makePkgModule (nameModuleWithInternal cm n)
      , tickThing = nameOccurenceText n
      -- If we pass in a SrcSpan, otherwise use a random one so that
      -- we can still emit references
      , tickSpan = maybe (cvtGhcSrcSpan (nameSrcSpan n)) Just  ss
      , tickUniqueInModule = isExternalName n
      , tickTermLevel = isValName n
      }


makeDeclTick :: Name -> Span -> Output Tick
makeDeclTick n ss = do
    {-
  let tickSourcePath = makeSourcePath sourcePath
      tickPkgModule  = makePkgModule (nameModuleWithInternal cm n)
      tickThing      = T.pack (getOccString n)
      tickSpan       = cvtGhcSrcSpan (nameSrcSpan n) --Just ss
      tickUniqueInModule = True
      -}

  sourcePath <- view outFile
  cm <- view curModule
  return $ nameInModuleToTick (Just ss) sourcePath cm n

nameOccurenceText :: Name -> Text
nameOccurenceText n = T.pack (getOccString n)

-- Module is only set for extenal things but we also want to set it for
-- internal ids.
nameModuleWithInternal :: Module -> Name -> Module
nameModuleWithInternal homeModule name = fromMaybe homeModule (nameModule_maybe name)


addBinder :: NamedThing b => b -> SrcSpan -> Output ()
addBinder b ss = binderMap %=
                  (\n -> extendNameEnv_C (\old new -> traceShow ("Warning overwriting",(getOccString b), old, new) new) n (getName b) (cvtSrcSpan ss))
-- This overwriting is not too bad as the binder map is only used to give
-- precise locations of binders. It seems to work ok.

-- Running the monad

initialState :: OutputState
initialState = OutputState (Pos 1 1) emptyNameEnv M.empty emptyNameEnv M.empty

runRender :: FilePath -> Module -> Output T.Text -> ((T.Text, OutputState), XRefs)
runRender fp m = runIdentity . runWriterT . flip runStateT initialState
                                        . flip runReaderT initialReader
  where
    initialReader = OutputReader fp m undefined

-- Final Conversion

output :: FilePath -> FilePath -> XRef -> IO ()
output infile outfile xref = do
  let baseVName = Raw.VName "" "core-kythe" "" "" "core-haskell"
  collect infile outfile baseVName xref


collect :: FilePath -> FilePath -> Raw.VName -> XRef -> IO ()
collect infile outfile baseVName xref = do
    sourceText <- lenientDecodeUtf8 infile
    writeFile outfile ""
    hoist (return . runIdentity) (toKythe baseVName sourceText xref
        -- Batch an ad-hoc number of entries to be emitted together.
        =$= chunksOf 1000)
        $$ sinkChunks
    --
  where
    sinkChunks :: Sink [Raw.Entry] IO ()
    sinkChunks = awaitForever (lift . sink)

    sink = mapM_ (\m -> do
        let wire = encodeMessage . Raw.toEntryProto $ m
        B.appendFile outfile . BL.toStrict . Builder.toLazyByteString
                 . varInt . B.length $ wire
        B.appendFile outfile wire)

-- | From proto-lens.
varInt :: Int -> Builder.Builder
varInt n
    | n < 128 = Builder.word8 (fromIntegral n)
    | otherwise = Builder.word8 (fromIntegral $ n .&. 127 .|. 128)
                    <> varInt (n `shiftR` 7)

-- Utility

singleton :: a -> [a]
singleton = (:[])

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f xs = catMaybes <$> mapM f xs

