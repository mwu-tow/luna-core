module Luna.Benchmark.Statistics where

import Prologue

import qualified Control.Lens.Aeson as Lens
import qualified Data.Yaml          as Yaml

-- TODO [AA] Actually use the more detailed statistics.



-----------------------
-- === TimeStats === --
-----------------------

-- === Definition === --

data TimeStats = TimeStats
    { _times   :: [Float]
    , _maxTime :: Float
    , _minTime :: Float
    , _avgTime :: Float
    , _stdTime :: Float
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



-----------------------
-- === TickStats === --
-----------------------

-- === Definition === --

data TickStats = TickStats
    { _tickCounts :: [Float]
    , _maxTicks   :: Float
    , _minTicks   :: Float
    , _avgTicks   :: Float
    , _stdTicks   :: Float
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



-----------------------
-- === MemStats === --
-----------------------

-- === Definition === --

data MemStats = MemStats
    { _memVals :: [Float]
    , _maxMem  :: Float
    , _minMem  :: Float
    , _avgMem  :: Float
    , _stdMem  :: Float
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



------------------------
-- === Statistics === --
------------------------

-- === Definition === --

data Statistics = Statistics
    { _locationName :: Text
    , _rawTimes     :: [Float]   -- Seconds
    , _rawTicks     :: [Integer] -- Ticks
    , _rawMem       :: [Integer] -- Bytes TODO [AA] Fix this.
    } deriving (Eq, Generic, Ord, Show)
makeLenses ''Statistics


-- === Instances === --

instance Default Statistics where
    def = Statistics def def def def

instance Yaml.FromJSON Statistics where
    parseJSON = Lens.parseYamlStyle

instance Yaml.ToJSON Statistics where
    toJSON     = Lens.toJSONYamlStyle
    toEncoding = Lens.toEncodingYamlStyle

