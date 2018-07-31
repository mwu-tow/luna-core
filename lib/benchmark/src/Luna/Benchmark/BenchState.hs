module Luna.Benchmark.BenchState where

import Prologue

import Data.Map.Strict (Map)
import Luna.Benchmark.Config       (Config)
import Luna.Benchmark.Statistics (Statistics)

-- TODO [AA] Functions to wrap the complexities of lookup and defaulting



------------------------
-- === BenchState === --
------------------------

-- === Definition === --

type LocKey = Text

data BenchState = BenchState
    { _currentTestName :: Text
    , _benchConfig     :: Config
    , _statsList       :: Map LocKey Statistics
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''BenchState


-- === Instances === --

instance Default BenchState where
    def = BenchState def def def

