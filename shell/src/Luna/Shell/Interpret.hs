module Luna.Shell.Interpret where

import Prologue

import qualified Data.Bimap                            as Bimap
import qualified Data.Graph.Data.Graph.Class           as Graph
import qualified Data.Map                              as Map
import qualified Luna.IR                               as IR
import qualified Luna.Pass.Evaluation.EvaluateUnits    as EvaluateUnits
import qualified Luna.Pass.Preprocess.PreprocessUnit   as PreprocessUnit
import qualified Luna.Pass.Resolve.Data.Resolution     as Res
import qualified Luna.Pass.Scheduler                   as Scheduler
import qualified Luna.Pass.Sourcing.Data.Unit          as Unit
import qualified Luna.Pass.Sourcing.UnitLoader         as ModLoader
import qualified Luna.Pass.Sourcing.UnitMapper         as UnitMap
import qualified Luna.Project                          as Project
import qualified Luna.Runtime                          as Runtime
import qualified Luna.Std                              as Std
import qualified Path                                  as Path
import qualified System.Directory                      as Directory

import Control.Monad.Exception               (MonadExceptions)
import Luna.Syntax.Text.Parser.Data.CodeSpan (CodeSpan)
import Path                                  (Path, Abs, Dir, File)
import System.IO                             (stderr, hPutStrLn)



-------------------------------
-- === Shell Interpreter === --
-------------------------------

-- === Definition === --
data ShellInterpreter


-- === Instances === --

type instance Graph.Components ShellInterpreter = '[IR.Terms, IR.Links]
type instance Graph.ComponentLayers ShellInterpreter IR.Links =
    '[IR.Target, IR.Source]
type instance Graph.ComponentLayers ShellInterpreter IR.Terms =
    '[IR.Users, IR.Model, IR.Type, CodeSpan]



---------------------------------
-- === Interpreter Harness === --
---------------------------------

-- === API === --

file :: ( MonadIO m, MonadThrow m
        , MonadExceptions '[Scheduler.Error, ModLoader.UnitLoadingError] m
        , MonadFix m )
     => Path Abs File -> m ()
file _ = putStrLn "Single-file mode is not yet supported."

project :: ( MonadIO m, MonadThrow m
           , MonadExceptions '[Scheduler.Error, ModLoader.UnitLoadingError] m
           , MonadFix m )
        => Path Abs Dir -> m ()
project projPath = do
    -- Swap the working directory
    originalDir <- liftIO $ Directory.canonicalizePath
               <$> Directory.getCurrentDirectory
    liftIO . Directory.setCurrentDirectory $ Path.fromAbsDir projPath

    Graph.encodeAndEval @ShellInterpreter $ Scheduler.evalT $ do
        projectRoot    <- fromJust projPath <$> Project.findProjectRoot projPath
        projectImports <- Project.projectImportPaths projectRoot
        importPaths    <- sequence $ Path.parseAbsDir . snd <$> projectImports
        projectSrcs    <- sequence $ Project.findProjectSources <$> importPaths

        let projSrcMap = Map.map Path.toFilePath $ foldl' Map.union Map.empty
                $ Bimap.toMapR <$> projectSrcs
            projectName  = Project.getProjectName projectRoot
            mainFileName = convert projectName <> "." <> Project.mainFileName

        ModLoader.init @ShellInterpreter
        (_, stdUnitRef) <- liftIO Std.stdlib
        Scheduler.registerAttr @Unit.UnitRefsMap
        Scheduler.setAttr $ Unit.UnitRefsMap
            $ Map.singleton "Std.Primitive" stdUnitRef
        ModLoader.loadUnit projSrcMap [] mainFileName
        for Std.stdlibImports $ ModLoader.loadUnit projSrcMap []
        Unit.UnitRefsMap mods <- Scheduler.getAttr

        units <- for mods $ \u -> case u ^. Unit.root of
            Unit.Graph r       -> UnitMap.mapUnit @ShellInterpreter r
            Unit.Precompiled u -> pure u

        let unitResolvers = Map.mapWithKey Res.resolverFromUnit units
            importResolvers = Map.mapWithKey (Res.resolverForUnit unitResolvers)
                $ over wrapped ("Std.Base" :) . over wrapped ("Std.Primitive" :)
                . view Unit.imports <$> mods

        for (Map.toList importResolvers) $ \(unitName, resolver) -> do
            case Map.lookup unitName units of
                Just uni -> PreprocessUnit.preprocessUnit @ShellInterpreter
                            resolver uni
                Nothing  -> liftIO $ hPutStrLn stderr $
                            "Unable to resolve compilation unit "
                            <> convert unitName

        computedUnits <- EvaluateUnits.evaluateUnits @ShellInterpreter units
        mainFunc      <- Runtime.lookupSymbol computedUnits mainFileName
            $ convert Project.mainFuncName

        putStrLn $ "Running " <> convert projectName <> " in interpreted mode."
        liftIO $ Runtime.runIO mainFunc

    -- Swap the working directory back
    _ <- liftIO $ Directory.setCurrentDirectory <$> originalDir

    pure ()

