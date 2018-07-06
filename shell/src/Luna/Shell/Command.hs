module Luna.Shell.Command where

import Prologue

import qualified Control.Monad.State.Layered       as State

-------------------------------
-- === Config State Monad == --
-------------------------------

-- === Definition === --
type ConfigStateIO m =
    ( MonadIO m
    , State.MonadStates '[] m)



----------------------------------
-- === Command Option Types === --
----------------------------------

-- === Definition === --

newtype Command = Run RunOpts deriving (Eq, Generic, Ord, Show)

data RunOpts = RunOpts
    { _file    :: FilePath
    , _project :: FilePath
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''RunOpts



-------------------------------
-- === Command Execution === --
-------------------------------

-- === API === --

-- TODO [Ara] Run based on inputs
run :: ConfigStateIO m => RunOpts -> m ()
run opts = liftIO $ print opts



-------------------------
-- === Luna Runner === --
-------------------------

-- === API === --

runLuna :: MonadIO m => Command -> m ()
runLuna command = case command of
        Run opts -> run opts

