module Luna.Shell.Command where

import Prologue

import qualified Control.Monad.State.Layered as State
import qualified Luna.Project                as Project
import qualified Path                        as Path
import qualified System.Directory            as Directory

import Path              (Path, Abs, Rel, File, Dir, (</>))
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

-- TODO [Ara] Run based on inputs
run :: ConfigStateIO m => RunOpts -> m ()
run (RunOpts target) = liftIO $ catch compute recover where
    compute =
        if not $ null target then do
            -- TODO [Ara] Parse the path
            -- TODO [Ara] Check is a file
            canonicalPath <- liftIO $ Directory.canonicalizePath target
            filePath <- Path.parseAbsFile canonicalPath
            putStrLn "File"
            print filePath
        else do
            cwd <- liftIO $ Directory.getCurrentDirectory
            pathCwd <- Path.parseAbsDir cwd
            putStrLn "Foo"

    -- TODO This can be done much better.
    recover (e :: SomeException) = hPutStrLn stderr $ displayException e



-------------------------
-- === Luna Runner === --
-------------------------

-- === API === --

runLuna :: (MonadIO m, MonadThrow m) => Command -> m ()
runLuna command = case command of
        Run opts -> run opts

interpretFile :: MonadIO m => Path Abs File -> m ()
interpretFile path = print path

interpretProject :: MonadIO m => Path Abs Dir -> m ()
interpretProject path = print path

