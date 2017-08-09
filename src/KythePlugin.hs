{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module KythePlugin(plugin) where

import Data.Text.Prettyprint.Doc
--import Data.Text.Prettyprint.Doc.Render.Util.StackMachine
import Data.Text.Prettyprint.Doc.Render.Util.SimpleDocTree
import Control.Monad.Writer hiding ((<>))
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad
import Control.Lens hiding ((<.>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.IO as T
import PprCore
import qualified FastString as FS
import Outputable (SDoc(..))
import OutputableAnnotation
import GhcPlugins (Plugin(..), defaultPlugin, CoreM, CoreToDo(..), CommandLineOption, ModGuts(..)
                  , getDynFlags, defaultUserStyle, initSDocContext, Module, OutputableBndr
                  , NamedThing(..), Name, isValName, RealSrcLoc(..))
import Name
import qualified SrcLoc as GHC
import Module hiding (getModule)
import qualified GhcPlugins as GHC ( getModule, Expr(..) )
import Language.Haskell.Indexer.Translate hiding (Pos(..))
import qualified Language.Haskell.Indexer.Translate as P (Pos(..))
import CoreSyn hiding (Expr (..))
import System.FilePath
import NameEnv
import Data.Maybe
import Debug.Trace
import Control.Concurrent.MVar (MVar)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Morph (lift, hoist)
--import qualified Data.ByteString as B
import Data.Conduit (($$), (=$=), await, awaitForever, yield, Conduit, Sink)

import qualified Language.Kythe.Schema.Raw as Raw
import qualified Language.Kythe.Schema.Raw.Proto as Raw
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as Builder

import Language.Haskell.Indexer.Frontend.Kythe
import Data.Conduit.List (chunksOf)
import Data.ProtoLens ( encodeMessage )
import Data.Bits

-- DataTypes

data Pos = Pos { _x :: Int, _y :: Int } deriving Show

data OutputState = OutputState { _pos :: Pos
                               , _nameMap :: NameEnv Tick
                               , _binderMap :: NameEnv Span
                                                }

data OutputReader = OutputReader { _binderPos :: Maybe Span
                                 , _outFile :: FilePath
                                 , _curModule :: Module }

data SrcSpan = SS Pos Pos FilePath deriving Show


makeLenses ''OutputState
makeLenses ''OutputReader
makeLenses ''Pos

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
        let path = SourcePath . transform . T.pack . FS.unpackFS . GHC.srcLocFile $ r
        in P.Pos (GHC.srcLocLine r) (GHC.srcLocCol r) path
      where transform = id -- aoFilePathTransform (ecOptions given)

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
doPrint outdir mgs@ModGuts{mg_binds, mg_module} = do
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
    let xref = toXRef mg_module outpath xrefs (view nameMap st)
    liftIO $ output outpath entriesPath pprint xref
    return mgs

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
makeModuleTick mod = ModuleTick
                      { mtPkgModule = makePkgModule mod
                      , mtSpan = Nothing }

makePkgModule :: Module -> PkgModule
makePkgModule (Module uid modname)
  = PkgModule { getPackage = T.pack (FS.unpackFS (unitIdFS uid))
              , getModule =  T.pack (moduleNameString modname)  }

makeSourcePath :: FilePath -> SourcePath
makeSourcePath fp = SourcePath (T.pack fp)


-- The intermediate type we use
data XRefs = XRefs [Decl]  -- Decls
                   (NameEnv Tick -> [TickReference]) -- Delayed references, the nameenv
                                                     -- is a map from decl
                                                     -- name to their ticks

showXrefs ::  XRefs -> NameEnv Tick -> String
showXrefs (XRefs ds nets)  ne = show (ds, nets ne)

toXRef :: Module -> FilePath -> XRefs -> NameEnv Tick -> XRef
toXRef mod fp (XRefs ds nets)  ne =
    XRef { xrefFile      = makeAnalysedFile fp
    , xrefModule    = makeModuleTick mod
    , xrefDecls     = ds
    , xrefCrossRefs = nets ne
    , xrefRelations = []
    , xrefImports   = []
    }

instance Monoid XRefs where
  mempty = XRefs [] (const [])
  (XRefs ds trs) `mappend` (XRefs ds' trs') = XRefs (ds ++ ds') (\d -> trs d ++ trs' d)

type Output a = ReaderT OutputReader (StateT OutputState (WriterT XRefs Identity)) a

render :: Doc PExpr -> Output T.Text
render ast = go (treeForm (layoutPretty defaultLayoutOptions ast))
  where
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
        STAnn ann rest -> do
          (ss, res) <- withSS (go rest)
          whatCore ann ss >>= tell
          return res


        STConcat xs -> fmap mconcat (traverse go xs)

-- Returns the Source span of the result of outputing the thing
withSS :: Output a -> Output (SrcSpan, a)
withSS o = do
  p <- use pos
  res <- o
  p' <- use pos
  out <- view outFile
  return ((SS p p' out), res)

whatCore :: PExpr -> SrcSpan -> Output XRefs
whatCore (PCoreExpr e) ss = return mempty
-- Need to particularlly deal with binders here
{-
  case e of
    GHC.Var {} -> "Var" -- Does this cause double wrapping?
    GHC.Lit {} -> "Lit"
    GHC.App {} -> "App"
    GHC.Lam {} -> "Lam"
    GHC.Let {} -> "Let"
    GHC.Case {} -> "Case"
    GHC.Cast {} -> "Cast"
    GHC.Tick {} -> "Tick"
    GHC.Type {} -> "Type"
    GHC.Coercion {} -> "Co"
-}
whatCore (PBind b) ss =
  (\d -> XRefs d (const [])) <$> makeDecl b ss
whatCore (PVar b v) ss  =
  case b of
    Binder ->   do
      addBinder v ss -- We already print out binding sites as decls but need
      return mempty
                                 -- to record exactly where the binder is
                                 -- for better highlighting
    Reference -> do
      outf <- view outFile
      cm <- view curModule
      let rt n = maybeToList (makeReferenceTick outf cm (getName v) (cvtSrcSpan ss) n)
      return $ XRefs [] rt -- References generate reference ticks

makeReferenceTick :: FilePath -> Module -> Name -> Span -> NameEnv Tick -> Maybe TickReference
makeReferenceTick outf cm n ss nenv =
  case lookupNameEnv nenv n of
    -- For debugging for now
    Nothing -> do
      -- Names without local definitions
      traceM ("Couldn't find name in nameenv: " ++ show (getOccString n) ++ show ss)
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
      in Just $ TickReference{..}


makeDecl :: forall b . (OutputableBndr b, NamedThing b) => Bind b -> SrcSpan -> Output [Decl]
makeDecl b  ss =
  case b of
    NonRec b eb -> singleton <$> go b eb
    Rec bs -> mapM (uncurry go) bs
  where
    go :: b -> GHC.Expr b -> Output Decl
    go b eb =
      let
          declIdentifierSpan = Just (cvtSrcSpan ss)
          declType = StringyType "" ""
          declExtra = Nothing
      in do
        declTick <- makeDeclTick (getName b) eb (cvtSrcSpan ss)
        nameMap %= (\n -> extendNameEnv n (getName b) declTick)
        declIdentifierSpan <- uses binderMap (\n -> lookupNameEnv n (getName b))
        return Decl{..}

nameInModuleToTick :: Maybe Span -> FilePath -> Module -> Name -> Tick
nameInModuleToTick ss sourcePath cm n = do
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

makeDeclTick n eb ss = do
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
addBinder b ss = binderMap %= (\n -> extendNameEnv n (getName b) (cvtSrcSpan ss))

-- Running the monad

initialState :: OutputState
initialState = OutputState (Pos 1 1) emptyNameEnv emptyNameEnv

runRender :: FilePath -> Module -> Output T.Text -> ((T.Text, OutputState), XRefs)
runRender fp m = runIdentity . runWriterT . flip runStateT initialState
                                        . flip runReaderT initialReader
  where
    initialReader = OutputReader Nothing fp m

-- Final Conversion

output :: FilePath -> FilePath -> Text -> XRef -> IO ()
output infile outfile pprint xref = do
  let baseVName = Raw.VName "" "" "" "" "core-haskell"
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
