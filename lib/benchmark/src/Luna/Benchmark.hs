module Luna.Benchmark where

import Prologue

import qualified Control.Monad.State.Layered as State
import qualified Criterion                   as Criterion
import qualified Luna.Benchmark.BenchState   as BenchState
import qualified Luna.Benchmark.Config       as Config
import qualified Luna.Benchmark.SrcLoc       as SrcLoc
import qualified Perf.Cycle                  as Perf
import qualified System.Directory            as Directory
import qualified System.FilePath             as FilePath
import qualified Weigh                       as Weigh

import Control.Monad.State.Layered (StateT)
import Luna.Benchmark.BenchState   (BenchState)
import Luna.Benchmark.Config       (Config)



--------------------
-- === BenchT === --
--------------------

-- === Definition === --

type BenchT m a = (MonadIO m, NFData a) => StateT BenchState m a

type Bench a = BenchT IO a

type MonadBench m = (Monad m, MonadIO m, State.Monad BenchState m)


-- === API === --

bench :: (NFData a, MonadIO m) => BenchT m a -> m (a, BenchState)
bench = benchWith (def @Config)

benchWith :: forall a m . (NFData a, MonadIO m) => Config -> BenchT m a
    -> m (a, BenchState)
benchWith cfg comp = do
    -- Perform environment setup to maintain accurate measurements
    liftIO . void $ Perf.warmup 100

    -- Evaluate the benchmark
    let initState = (def @BenchState) & BenchState.benchConfig .~ cfg
    ret@(_, finalState) <- State.runT comp initState

    -- Cleanup and post-processing of results + Display
    -- TODO [AA] Display using `layouting`
    pure ret
{-# NOINLINE benchWith #-}



-----------------
-- === API === --
-----------------

-- Sample usage for now
test2 :: IO ()
test2 = void $ bench test

test :: Bench Int
test = tick "Test" id (1 :: Int)

tick :: forall a b m . (NFData a, MonadBench m) => Text -> (b -> a) -> b -> m a
tick label !f !b = do
    numTests <- (^. (BenchState.benchConfig . Config.numRuns))
        <$> State.get @BenchState

    ([cycles], result) <- liftIO $ Perf.ticks numTests f b

    -- TODO [AA] Merge the data.
    -- TODO [AA] Need to handle the case where there is no data properly.
    -- TODO [AA] Logic to warn about duplicate keys.

    pure result
{-# INLINE tick #-}

time :: forall a b m . (NFData a, MonadBench m) => Text -> (b -> a) -> b -> m a
time label !f !b = undefined
{-# INLINE time #-}

mem :: forall a b m . (NFData a, MonadBench m) => Text -> (b -> a) -> b -> m a
mem label !f !b = undefined
{-# INLINE mem #-}

-- measure (fn to do all of the above without issues)

