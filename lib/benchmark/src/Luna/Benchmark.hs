module Luna.Benchmark where

import Prologue

import qualified Control.Monad.State.Layered as State
import qualified Luna.Benchmark.Internal     as Internal
import qualified Luna.Benchmark.Statistics   as Statistics

import Luna.Benchmark.Statistics (Statistics)



-----------------------------
-- === Benchmark State === --
-----------------------------

-- === Definition === --

newtype BenchState = BenchState
    { _statsList :: [Statistics]
    } deriving (Eq, Generic, Ord, Show)


-- === Instances === --

instance Default BenchState where
    def = BenchState def



--------------------
-- === MonadBench === --
--------------------

-- === Definition === --

-- TODO [AA] Layered State here
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

