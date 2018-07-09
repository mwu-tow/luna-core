module Luna.Shell.Interpret where

import Prologue

import qualified Control.Concurrent.Async              as Async
import qualified Control.Monad.Exception               as Exception
import qualified Control.Monad.State                   as State
import qualified Control.Monad.State.Layered           as State
import qualified Data.Bimap                            as Bimap
import qualified Data.Graph.Component.Node.Destruction as Destruction
import qualified Data.Graph.Data.Component.Vector      as ComponentVector
import qualified Data.Graph.Data.Graph.Class           as Graph
import qualified Data.Graph.Data.Graph.Class           as Graph
import qualified Data.Graph.Data.Layer.Layout          as Layout
import qualified Data.List                             as List
import qualified Data.Map                              as Map
import qualified Data.Set                              as Set
import qualified Data.Tag                              as Tag
import qualified Data.Text                             as Text
import qualified Luna.Debug.IR.Visualizer              as Vis
import qualified Luna.IR                               as IR
import qualified Luna.IR.Aliases                       as Uni
import qualified Luna.IR.Layer                         as Layer
import qualified Luna.Pass                             as Pass
import qualified Luna.Pass.Attr                        as Attr
import qualified Luna.Pass.Basic                       as Pass
import qualified Luna.Pass.Data.Root                   as Root
import qualified Luna.Pass.Data.UniqueNameGen          as UniqueNameGen
import qualified Luna.Pass.Evaluation.EvaluateUnits    as EvaluateUnits
import qualified Luna.Pass.Evaluation.Interpreter      as Interpreter
import qualified Luna.Pass.Preprocess.PreprocessDef    as PreprocessDef
import qualified Luna.Pass.Preprocess.PreprocessUnit   as PreprocessUnit
import qualified Luna.Pass.Resolve.AliasAnalysis       as AliasAnalysis
import qualified Luna.Pass.Resolve.Data.Resolution     as Res
import qualified Luna.Pass.Scheduler                   as Scheduler
import qualified Luna.Pass.Sourcing.Data.Class         as Class
import qualified Luna.Pass.Sourcing.Data.Def           as Def
import qualified Luna.Pass.Sourcing.Data.Unit          as Unit
import qualified Luna.Pass.Sourcing.UnitLoader         as ModLoader
import qualified Luna.Pass.Sourcing.UnitMapper         as UnitMap
import qualified Luna.Project                          as Project
import qualified Luna.Runtime                          as Runtime
import qualified Luna.Runtime.Data.Future              as Future
import qualified Luna.Std                              as Std
import qualified Luna.Syntax.Text.Parser.Data.Result   as Parser
import qualified Luna.Syntax.Text.Parser.Pass          as Parser
import qualified Luna.Syntax.Text.Source               as Parser
import qualified OCI.Data.Name.Class                   as Name
import qualified OCI.Pass.Definition.Interface         as Pass
import qualified Path                                  as Path
import qualified Text.PrettyPrint.ANSI.Leijen          as Doc

import Control.Monad.Exception               (MonadExceptions)
import Data.Graph.Data.Component.Class       (unsafeNull)
import Data.Map                              (Map)
import Data.Set                              (Set)
import Luna.Pass                             (Pass)
import Luna.Syntax.Text.Parser.Data.CodeSpan (CodeSpan)
import Luna.Syntax.Text.Parser.Data.Invalid  (Invalids)
import Path                                  (Path, Abs, Dir, File)



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
        , MonadExceptions '[Scheduler.Error, ModLoader.UnitLoadingError] m )
     => Path Abs File -> m ()
file _ = putStrLn "Single-file mode is not yet supported."

project :: ( MonadIO m, MonadThrow m
           , MonadExceptions '[Scheduler.Error, ModLoader.UnitLoadingError] m )
        => Path Abs Dir -> m ()
project projPath = Graph.encodeAndEval @ShellInterpreter $ Scheduler.evalT $ do
    projectRoot    <- fromJust projPath <$> Project.findProjectRoot projPath
    projectDeps    <- Project.listDependencies projectRoot
    projectImports <- Project.projectImportPaths projectRoot
    importPaths    <- sequence $ Path.parseAbsDir . snd <$> projectImports
    projectSrcs    <- sequence $ Project.findProjectSources <$> importPaths

    let projSrcMap = Map.map Path.toFilePath $ foldl' Map.union Map.empty
            $ Bimap.toMapR <$> projectSrcs
        projectName  = Project.getProjectName projectRoot
        mainFileName :: IR.Qualified
        mainFileName = Project.mainFileName

    print projSrcMap

    -- Set up the interpreter
    ModLoader.init @ShellInterpreter
    (finalise, stdUnitRef) <- liftIO Std.stdlib
    Scheduler.registerAttr @Unit.UnitRefsMap
    Scheduler.setAttr $ Unit.UnitRefsMap
        $ Map.singleton "Std.Primitive" stdUnitRef
    ModLoader.loadUnit projSrcMap [] mainFileName

    putStrLn $ "Running " <> convert projectName <> " in interpreted mode."

    pure ()

