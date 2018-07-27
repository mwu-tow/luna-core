module Luna.Benchmark where

import Prologue

import qualified Control.Monad.State.Layered as State
import qualified Perf                        as Perf
import qualified Weigh                       as Weigh
import qualified Criterion                   as Criterion

import Data.Map.Strict           (Map)
import Luna.Benchmark.Statistics (Statistics)



-----------------------------
-- === BenchState === --
-----------------------------

-- === Definition === --

type LocKey = Text

data BenchState = BenchState
    { _currentTestName :: Text
    , _statsList       :: Map LocKey Statistics
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''BenchState


-- === Instances === --

instance Default BenchState where
    def = BenchState def def



--------------------
-- === MonadBench === --
--------------------

-- === Definition === --

-- TODO [AA] Need to make this a Monad Transformer
type MonadBench m = (MonadIO m, State.Monad BenchState m)



-----------------
-- === API === --
-----------------

time :: forall a m . MonadBench m => a -> m a
time = undefined

ticks :: forall a m . MonadBench m => a -> m a
ticks = undefined

mem :: forall a m . MonadBench m => a -> m a
mem = undefined

