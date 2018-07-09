module Luna.Shell.Interpret where

import Prologue

import Luna.Project as Project

import Path (Path, Abs, Dir, File)

---------------------------------
-- === Interpreter Harness === --
---------------------------------

-- === API === --

file :: MonadIO m => Path Abs File -> m ()
file _ = putStrLn "Single-file mode is not yet supported."

project :: MonadIO m => Path Abs Dir -> m ()
project path = do
    print path
    putStrLn $ "Interpreting " <> show path

