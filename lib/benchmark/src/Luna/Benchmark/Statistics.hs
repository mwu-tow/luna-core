module Luna.Benchmark.Statistics where

import Prologue

import qualified Control.Lens.Aeson                 as Lens
import qualified Data.Yaml                          as Yaml
import qualified Luna.Benchmark.Statistics.Internal as Internal

import Luna.Benchmark.SrcLoc (SrcLoc)
import Perf                  (Cycle)



-----------------------
-- === TimeStats === --
-----------------------

-- === Definition === --

data TimeStats = TimeStats
    { _times   :: {-# UNPACK #-} ![Double] -- in seconds
    , _maxTime :: {-# UNPACK #-} !Double
    , _minTime :: {-# UNPACK #-} !Double
    , _avgTime :: {-# UNPACK #-} !Double
    , _stdTime :: {-# UNPACK #-} !Double
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''TimeStats


-- === Instances === --

instance Default TimeStats where
    def = TimeStats def def def def def

instance Yaml.FromJSON TimeStats where
    parseJSON = Lens.parseYamlStyle

instance Yaml.ToJSON TimeStats where
    toJSON     = Lens.toJSONYamlStyle
    toEncoding = Lens.toEncodingYamlStyle

instance Semigroup TimeStats where
    (TimeStats t1 max1 min1 _ _) <> (TimeStats t2 max2 min2 _ _)
        = TimeStats times (max max1 max2) (min min1 min2) (Internal.meanF times)
                (Internal.stddevF times) where times = t1 <> t2
    {-# INLINE (<>) #-}

instance StyledShow Pretty TimeStats where
    styledShow _ (TimeStats times max min avg stddev)
        =  "Recorded Times: "     <> convert (show times)
        <> "Max Time: "           <> convert (show max)
        <> "Min Time: "           <> convert (show min)
        <> "Mean Time: "          <> convert (show avg)
        <> "Standard Deviation: " <> convert (show stddev)


-----------------------
-- === TickStats === --
-----------------------

-- === Definition === --

data TickStats = TickStats
    { _tickCounts :: {-# UNPACK #-} ![Cycle] -- In ticks
    , _maxTicks   :: {-# UNPACK #-} !Cycle
    , _minTicks   :: {-# UNPACK #-} !Cycle
    , _avgTicks   :: {-# UNPACK #-} !Cycle
    , _stdTicks   :: {-# UNPACK #-} !Double
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''TickStats


-- === Instances === --

instance Default TickStats where
    def = TickStats def def def def def

instance Yaml.FromJSON TickStats where
    parseJSON = Lens.parseYamlStyle

instance Yaml.ToJSON TickStats where
    toJSON     = Lens.toJSONYamlStyle
    toEncoding = Lens.toEncodingYamlStyle

instance Semigroup TickStats where
    (TickStats t1 max1 min1 _ _) <> (TickStats t2 max2 min2 _ _)
        = TickStats ticks (max max1 max2) (min min1 min2) (Internal.meanI ticks)
            (Internal.stddevI ticks) where ticks = t1 <> t2
    {-# INLINE (<>) #-}

instance StyledShow Pretty TickStats where
    styledShow _ (TickStats times max min avg stddev)
        =  "Recorded Ticks: "     <> convert (show times)
        <> "Max Ticks: "          <> convert (show max)
        <> "Min Ticks: "          <> convert (show min)
        <> "Mean Ticks: "         <> convert (show avg)
        <> "Standard Deviation: " <> convert (show stddev)



-----------------------
-- === MemStats === --
-----------------------

-- === Definition === --

data MemStats = MemStats
    { _memVals :: {-# UNPACK #-} ![Int64] -- In Bytes
    , _maxMem  :: {-# UNPACK #-} !Int64
    , _minMem  :: {-# UNPACK #-} !Int64
    , _avgMem  :: {-# UNPACK #-} !Int64
    , _stdMem  :: {-# UNPACK #-} !Double
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''MemStats


-- === Instances === --

instance Default MemStats where
    def = MemStats def def def def def

instance Yaml.FromJSON MemStats where
    parseJSON = Lens.parseYamlStyle

instance Yaml.ToJSON MemStats where
    toJSON     = Lens.toJSONYamlStyle
    toEncoding = Lens.toEncodingYamlStyle

instance Semigroup MemStats where
    (MemStats m1 max1 min1 _ _) <> (MemStats m2 max2 min2 _ _)
        = MemStats mem (max max1 max2) (min min1 min2) (Internal.meanI mem)
            (Internal.stddevI mem) where mem = m1 <> m2
    {-# INLINE (<>) #-}

instance StyledShow Pretty MemStats where
    styledShow _ (MemStats times max min avg stddev)
        =  "Recorded Allocations: " <> convert (show times)
        <> "Max Bytes: "            <> convert (show max)
        <> "Min Bytes: "            <> convert (show min)
        <> "Mean Bytes: "           <> convert (show avg)
        <> "Standard Deviation: "   <> convert (show stddev)



------------------------
-- === Statistics === --
------------------------

-- === Definition === --

data Statistics = Statistics
    { _locationName :: !Text
    , _locationInfo :: !SrcLoc
    , _timeInfo     :: !TimeStats
    , _tickInfo     :: !TickStats
    , _memInfo      :: !MemStats
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''Statistics


-- === API === --

-- TODO [AA] Comparison functions with thresholds in IO
compare :: MonadIO m => Statistics -> Statistics -> m ()
compare _ _ = undefined


-- === Instances === --

instance Default Statistics where
    def = Statistics def def def def def

instance Yaml.FromJSON Statistics where
    parseJSON = Lens.parseYamlStyle

instance Yaml.ToJSON Statistics where
    toJSON     = Lens.toJSONYamlStyle
    toEncoding = Lens.toEncodingYamlStyle

instance Semigroup Statistics where
    (Statistics l1 i1 time1 tick1 mem1) <> (Statistics _ _ time2 tick2 mem2)
        = Statistics l1 i1 (time1 <> time2) (tick1 <> tick2) (mem1 <> mem2)
    {-# INLINE (<>) #-}

-- TODO [AA] Make this nice.
instance StyledShow Pretty Statistics where
    styledShow _ (Statistics name _ _ _ _) = name

