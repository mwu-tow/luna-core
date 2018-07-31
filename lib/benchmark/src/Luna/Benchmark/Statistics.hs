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
    (TimeStats c1 max1 min1 avg1 std1) <> (TimeStats c2 max2 min2 avg2 std2)
        = undefined
    {-# INLINE (<>) #-}



-----------------------
-- === TickStats === --
-----------------------

-- === Definition === --

data TickStats = TickStats
    { _tickCounts :: {-# UNPACK #-} ![Cycle] -- In ticks
    , _maxTicks   :: {-# UNPACK #-} !Cycle
    , _minTicks   :: {-# UNPACK #-} !Cycle
    , _avgTicks   :: {-# UNPACK #-} !Cycle
    , _stdTicks   :: {-# UNPACK #-} !Cycle
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
    (TickStats c1 max1 min1 avg1 std1) <> (TickStats c2 max2 min2 avg2 std2)
        = undefined
    {-# INLINE (<>) #-}



-----------------------
-- === MemStats === --
-----------------------

-- === Definition === --

data MemStats = MemStats
    { _memVals :: {-# UNPACK #-} ![Int64] -- In Bytes
    , _maxMem  :: {-# UNPACK #-} !Int64
    , _minMem  :: {-# UNPACK #-} !Int64
    , _avgMem  :: {-# UNPACK #-} !Int64
    , _stdMem  :: {-# UNPACK #-} !Int64
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
    (MemStats c1 max1 min1 avg1 std1) <> (MemStats c2 max2 min2 avg2 std2)
        = undefined
    {-# INLINE (<>) #-}



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

