module Luna.Benchmark where

import Prologue

import qualified Control.Monad.State.Layered as State
import qualified Luna.Benchmark.SrcLoc       as SrcLoc
import qualified Perf                        as Perf
import qualified Weigh                       as Weigh
import qualified Criterion                   as Criterion

import Control.Monad.State.Layered (StateT)
import Data.Map.Strict             (Map)
import Luna.Benchmark.Statistics   (Statistics)



------------------------
-- === BenchState === --
------------------------

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
-- === BenchT === --
--------------------

-- === Definition === --

type BenchT m a = StateT BenchState m a

type Bench a = BenchT Identity a

type MonadBench s m = (Monad m, MonadIO m, State.Monad s m)


-- === API === --



-----------------
-- === API === --
-----------------

time :: forall a m . MonadBench BenchState m => a -> m a
time = undefined

ticks :: forall a m . MonadBench BenchState m => a -> m a
ticks = undefined

mem :: forall a m . MonadBench BenchState m => a -> m a
mem = undefined
