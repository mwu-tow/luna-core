module Luna.Benchmark where

import Prologue

import qualified Control.Monad.State.Layered as State
import qualified Luna.Benchmark.Config       as Config
import qualified Luna.Benchmark.SrcLoc       as SrcLoc
import qualified Perf.Cycle                  as Perf
import qualified Weigh                       as Weigh
import qualified Criterion                   as Criterion

import Control.Monad.State.Layered (StateT)
import Data.Map.Strict             (Map)
import Luna.Benchmark.Config       (Config)
import Luna.Benchmark.Statistics   (Statistics)



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
    let initState = (def @BenchState) & benchConfig .~ cfg
    ret@(res, finalState) <- State.runT comp initState

    -- Cleanup and post-processing of results
    pure ret


-----------------
-- === API === --
-----------------

-- Sample usage for now
test2 :: IO ()
test2 = void $ bench test

test :: Bench Int
test = tick "Test" id (1 :: Int)

tick :: forall a b m . (NFData a, MonadBench m) => Text -> (b -> a) -> b -> m a
tick label f b = do
    liftIO . void $ Perf.warmup 100
    st <- State.get @BenchState
    pure $ f b

-- time :: forall a m . (NFData a, MonadBench BenchState m) => a -> m a
-- time = undefined
--
-- mem :: forall a m . (NFData a, MonadBench BenchState m) => a -> m a
-- mem = undefined

-- measure (fn to do all of the above without issues)

