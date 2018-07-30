module Luna.Benchmark.Statistics where

import Prologue

import qualified Control.Lens.Aeson    as Lens
import qualified Data.Yaml             as Yaml

import Luna.Benchmark.SrcLoc (SrcLoc)



-----------------------
-- === TimeStats === --
-----------------------

-- === Definition === --

data TimeStats = TimeStats
    { _times   :: [Float] -- in seconds
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
    { _tickCounts :: [Integer] -- In ticks
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
    { _memVals :: [Integer] -- In Bytes
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
    , _locationInfo :: SrcLoc
    , _timeInfo     :: TimeStats
    , _tickInfo     :: TickStats
    , _memInfo      :: MemStats
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

