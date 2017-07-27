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
import qualified Data.Text.IO as T
import PprCore
import Outputable (SDoc(..))
import OutputableAnnotation
import GhcPlugins (Plugin(..), defaultPlugin, CoreM, CoreToDo(..), CommandLineOption, ModGuts(..)
                  , getDynFlags, defaultUserStyle, initSDocContext, Module, OutputableBndr
                  , NamedThing(..), Name)
import Name
import Module hiding (getModule)
import qualified GhcPlugins as GHC ( Expr(..) )
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
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as Builder

data Pos = Pos { _x :: Int, _y :: Int } deriving Show

data OutputState = OutputState { _pos :: Pos, _nameMap :: NameEnv Tick }

data OutputReader = OutputReader { _binderPos :: Maybe Span, _outFile :: FilePath }

data SrcSpan = SS Pos Pos FilePath deriving Show


makeLenses ''OutputState
makeLenses ''OutputReader
makeLenses ''Pos

cvtSrcSpan :: SrcSpan -> Span
cvtSrcSpan (SS (Pos x1 y1) (Pos x2 y2) fp) =
  let cfp = makeSourcePath fp
  in Span (P.Pos y1 x1 cfp) (P.Pos y2 x2 cfp)

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
    let outpath = (makeModulePath outdir mg_module)
    liftIO $ putStrLn outpath
    dflags <- getDynFlags
    let ctx = initSDocContext dflags (defaultUserStyle dflags)
        sdoc = runSDoc (pprCoreBindingsWithAnn mg_binds)
    liftIO $ putStrLn "Running printer"
    let ((pprint, st), xrefs) = runRender outpath (render (sdoc ctx))
    liftIO $ T.putStrLn pprint
    liftIO $ putStrLn $ showXrefs xrefs (view nameMap st)
    liftIO $ putStrLn (show (toXRef mg_module outpath xrefs (view nameMap st)))
    return mgs

makeModulePath :: FilePath -> Module -> FilePath
makeModulePath fp md = fp </> modName <.> "core"
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
makePkgModule ~(Module _ modname)
  = PkgModule { getPackage = "", getModule =  ""  } -- T.pack (moduleNameString modname) }

makeSourcePath :: FilePath -> SourcePath
makeSourcePath fp = SourcePath (T.pack fp)

data AST = Var Int | Add AST AST

pprAST :: AST -> Doc AST
pprAST a = annotate a (pprASTBasic a)

pprASTBasic :: AST -> Doc AST
pprASTBasic (Var n) = pretty n
pprASTBasic (Add l r) = parens (pprAST l) <+> "+"
                                               <+> parens (pprAST r)

data XRefs = XRefs [Decl] (NameEnv Tick -> [TickReference])

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

ds :: AST -> SimpleDocStream AST
ds ast = layoutPretty defaultLayoutOptions (pprAST ast)

render :: Doc PExpr -> Output Text
render ast = go (treeForm (layoutPretty defaultLayoutOptions ast))
  where
    go tf =
      case tf of
        STEmpty -> pure mempty
        STChar c -> do pos . x %= (+1)
                       pure (T.singleton c)
        STText l t -> do
                        pos . x %= (+ l)
                        return t
        STLine i -> do
                      pos . y %= (+1)
                      pos . x .= i
                      return (T.singleton '\n' <> T.replicate i " ")
        STAnn ann rest -> do
          p <- use pos
          res <- go rest
          p' <- use pos
          out <- view outFile
          whatCore ann (SS p p' out) >>= tell
          return res


        STConcat xs -> fmap mconcat (traverse go xs)

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
    Binder -> return mempty -- local binderPos ss -- We already print out binding sites as decls but need
                                 -- to record exactly where the binder is
                                 -- for better highlighting
    Reference ->
      let rt n = maybeToList (makeReferenceTick (getName v) (cvtSrcSpan ss) n)
      in return $ XRefs [] rt -- References generate reference ticks

makeReferenceTick :: Name -> Span -> NameEnv Tick -> Maybe TickReference
makeReferenceTick n ss nenv =
  case lookupNameEnv nenv n of
    -- For debugging for now
    Nothing -> trace ("Couldn't find name in nameenv: " ++ show (getOccString n) ++ show ss) Nothing
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
      let declTick = makeDeclTick (getName b) eb (cvtSrcSpan ss)
          declIdentifierSpan = Nothing
          declType = StringyType "" ""
          declExtra = Nothing
      in do
        nameMap %= (\n -> extendNameEnv n (getName b) declTick)
        declIdentifierSpan <- view binderPos -- Need to refine this to a map probably?
        return Decl{..}

makeDeclTick n eb ss =
  let tickSourcePath = SourcePath "Pass-In"
      tickPkgModule  = makePkgModule (nameModule n)
      tickThing      = T.pack (getOccString n)
      tickSpan       = Just ss
      tickUniqueInModule = True
      tickTermLevel = True
  in Tick{..}

singleton = (:[])



initialState :: OutputState
initialState = OutputState (Pos 0 0) emptyNameEnv

runRender :: FilePath -> Output Text -> ((Text, OutputState), XRefs)
runRender fp = runIdentity . runWriterT . flip runStateT initialState
                                        . flip runReaderT initialReader
  where
    initialReader = OutputReader Nothing fp

-- Final Conversion


collect baseVName sourceText xref = do
    -- Note: since this Conduit pipeline is pretty context-dependent, there
    -- is low chance of leak due to accidental sharing (see
    -- https://www.well-typed.com/blog/2016/09/sharing-conduit/).
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
        B.putStr . BL.toStrict . Builder.toLazyByteString
                 . varInt . B.length $ wire
        B.putStr wire)
