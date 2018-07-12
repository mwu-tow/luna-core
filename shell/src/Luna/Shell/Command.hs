module Luna.Shell.Command where

import Prologue

import qualified Control.Monad.State.Layered as State
import qualified Luna.Project                as Project
import qualified Luna.Shell.CWD              as CWD
import qualified Luna.Shell.Interpret        as Interpret
import qualified Path                        as Path
import qualified System.Directory            as Directory

import System.IO         (hPutStrLn, stderr)


-------------------------------
-- === Config State Monad == --
-------------------------------

-- === Definition === --
type ConfigStateIO m =
    ( MonadIO m
    , MonadThrow m
    , State.MonadStates '[] m)



----------------------------------
-- === Command Option Types === --
----------------------------------

-- === Definition === --

newtype RunOpts = RunOpts
    { _target :: FilePath
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''RunOpts

data InitOpts = InitOpts
    { _name        :: String
    , _lunaVersion :: String
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''InitOpts

data BuildOpts = BuildOpts
    { __acquireDeps        :: Bool
    , __cleanBuild         :: Bool
    , __standaloneFileName :: String
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''BuildOpts

data TestOpts = TestOpts
    { __doNotBuild  :: Bool
    , __noBenchmark :: Bool
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''TestOpts

data CleanOpts = CleanOpts
    { __full  :: Bool
    , __docs  :: Bool
    , __cache :: Bool
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''CleanOpts

data PublishOpts = PublishOpts
    { __bumpMajor :: Bool
    , __bumpMinor :: Bool
    , __bumpPatch :: Bool
    , __prerelease :: String
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''PublishOpts

newtype RetractOpts = RetractOpts
    { __version :: String
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''RetractOpts

newtype OptionOpts = OptionOpts
    { __options :: [String]
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''OptionOpts

newtype RollbackOpts = RollbackOpts
    { __hash :: String
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''RollbackOpts

newtype UpdateOpts = UpdateOpts
    { __dependencyName :: [String]
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''UpdateOpts

newtype FreezeOpts = FreezeOpts
    { __dependencyName :: [String]
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''FreezeOpts

newtype InstallOpts = InstallOpts
    { __packages :: [String]
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''InstallOpts

newtype DownloadOpts = DownloadOpts
    { __packages :: [String]
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''DownloadOpts

data Command
    = Build BuildOpts
    | Clean CleanOpts
    | Doc
    | Download DownloadOpts
    | Freeze FreezeOpts
    | Init InitOpts
    | Install InstallOpts
    | Options OptionOpts
    | Publish PublishOpts
    | Retract RetractOpts
    | Rollback RollbackOpts
    | Run RunOpts
    | Test TestOpts
    | Unfreeze FreezeOpts
    | Update UpdateOpts
    deriving (Eq, Generic, Ord, Show)



-------------------------------
-- === Command Execution === --
-------------------------------

-- === API === --

run :: ConfigStateIO m => RunOpts -> m ()
run (RunOpts target) = liftIO $ catch compute recover where
    compute =
        if not $ null target then do
            canonicalPath <- liftIO $ Directory.canonicalizePath target
            fileExists    <- liftIO $ Directory.doesFileExist canonicalPath
            projectExists <- liftIO $ Directory.doesDirectoryExist canonicalPath

            if fileExists then do
                filePath <- Path.parseAbsFile canonicalPath
                if Path.fileExtension filePath /= Project.lunaFileExt then
                    hPutStrLn stderr $ canonicalPath <> " is not a Luna file."
                else Interpret.file filePath
            else if projectExists then runProject canonicalPath
            else hPutStrLn stderr $ target <> " not found."
        else do
            cwd <- CWD.get
            runProject cwd

    -- FIXME This can be done much better.
    recover (e :: SomeException) = hPutStrLn stderr $ displayException e

    runProject path = do
        projectPath   <- Path.parseAbsDir path
        isLunaProject <- Project.isLunaProject projectPath

        if isLunaProject then Interpret.project projectPath
        else hPutStrLn stderr $ path <> " is not a Luna Project."



-------------------------
-- === Luna Runner === --
-------------------------

-- === API === --

runLuna :: (MonadIO m, MonadThrow m) => Command -> m ()
runLuna command = case command of
        Build    _ -> putStrLn "Building of executables is not yet implemented."
        Clean    _ -> putStrLn "Cleaning build artefacts is not yet implemented."
        Doc        -> putStrLn "Building documentation is not yet implemented."
        Download _ -> putStrLn "Downloading of projects is not yet implemented."
        Freeze   _ -> putStrLn
            "Freezing project dependencies is not yet implemented."
        Init     _ -> putStrLn "Creating new projects is not yet implemented."
        Install  _ -> putStrLn "Installing dependencies is not yet implemented."
        Options  _ -> putStrLn "Setting compiler options is not yet implemented."
        Publish  _ -> putStrLn "Publishing projects is not yet implemented."
        Retract  _ -> putStrLn
            "Retraction of project versions is not yet implemented."
        Rollback _ -> putStrLn "Rolling back dependencies is not yet implemented."
        Run opts   -> run opts
        Test     _ -> putStrLn "Executing test suites is not yet implemented."
        Unfreeze _ -> putStrLn
            "Unfreezing project dependencies is not yet implemented."
        Update   _ -> putStrLn
            "Updating project dependencies is not yet implemented."

