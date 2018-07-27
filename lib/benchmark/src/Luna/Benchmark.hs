module Luna.Benchmark where

import Prologue

import qualified Control.Monad.State.Layered as State
import qualified Luna.Benchmark.Internal     as Internal
import qualified Luna.Benchmark.Statistics   as Statistics

import Data.Map.Strict           (Map)
import Luna.Benchmark.Statistics (Statistics)



-----------------------------
-- === Benchmark State === --
-----------------------------

-- === Definition === --

type LocKey = Text

newtype BenchState = BenchState
    { _statsList :: Map LocKey Statistics
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''BenchState


-- === Instances === --

instance Default BenchState where
    def = BenchState def



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

