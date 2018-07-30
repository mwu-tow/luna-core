module Luna.Benchmark.Config where

import Prologue

import System.FilePath (FilePath)



--------------------
-- === Config === --
--------------------

data Config = Config
    { _numRuns :: Int
    , _outputPath :: FilePath
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''Config


-- === Instances === --

instance Default Config where
    def = Config 5 "."

