module Luna.Shell.Command where

import Prologue

import qualified Control.Monad.State.Layered as State
import qualified Luna.Project                as Project
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

newtype Command = Run RunOpts deriving (Eq, Generic, Ord, Show)

newtype RunOpts = RunOpts
    { _target :: FilePath
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''RunOpts



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
            cwd <- liftIO Directory.getCurrentDirectory
            runProject cwd

    -- TODO This can be done much better.
    recover (e :: SomeException) = hPutStrLn stderr $ displayException e

    runProject path = do
        projectPath <- Path.parseAbsDir path
        isLunaProject <- Project.isLunaProject projectPath

        if isLunaProject then Interpret.project projectPath
        else hPutStrLn stderr $ path <> " is not a Luna Project."



-------------------------
-- === Luna Runner === --
-------------------------

-- === API === --

runLuna :: (MonadIO m, MonadThrow m) => Command -> m ()
runLuna command = case command of
        Run opts -> run opts

