module Luna.Benchmark.Config where

import Prologue

import qualified Path as Path

import Path (Path, Rel, Dir)



--------------------
-- === Config === --
--------------------

data Config = Config
    { _numRuns      :: !Int
    , _outputPath   :: !(Path Rel Dir)
    , _historyCount :: !Int
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''Config


-- === Instances === --

instance Default Config where
    def = Config 5 $(Path.mkRelDir "./bench-results") 1

