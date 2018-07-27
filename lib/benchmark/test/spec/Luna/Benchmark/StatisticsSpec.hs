module Luna.Benchmark.StatisticsSpec where

import Prologue

import qualified Luna.Benchmark.Statistics as Statistics

import Data.ByteString           ( ByteString )
import Luna.Benchmark.Statistics ( Statistics(Statistics) )
import Luna.YamlUtils.Test       ( shouldDecodeAs, shouldGenerate
                                 , shouldNotDecode )
import Test.Hspec                ( Spec, describe, it )



-----------------------
-- === Test Data === --
-----------------------

statsEx1 :: Statistics
statsEx1 = def @Statistics

statsEx1Yaml :: ByteString
statsEx1Yaml = [qqStr|
location-name: ''
location-info:
  src-loc-file: ''
  src-loc-start-col: 0
  src-loc-end-line: 0
  src-loc-package: ''
  src-loc-module: ''
  src-loc-end-col: 0
  src-loc-start-line: 0
tick-info:
  std-ticks: 0
  min-ticks: 0
  max-ticks: 0
  tick-counts: []
  avg-ticks: 0
time-info:
  max-time: 0
  avg-time: 0
  std-time: 0
  min-time: 0
  times: []
mem-info:
  min-mem: 0
  mem-vals: []
  std-mem: 0
  max-mem: 0
  avg-mem: 0
|]

statsList :: [Statistics]
statsList = [ def @Statistics, def @Statistics, def @Statistics ]

statsListYaml :: ByteString
statsListYaml = [qqStr|
- location-name: ''
  location-info:
    src-loc-file: ''
    src-loc-start-col: 0
    src-loc-end-line: 0
    src-loc-package: ''
    src-loc-module: ''
    src-loc-end-col: 0
    src-loc-start-line: 0
  tick-info:
    std-ticks: 0
    min-ticks: 0
    max-ticks: 0
    tick-counts: []
    avg-ticks: 0
  time-info:
    max-time: 0
    avg-time: 0
    std-time: 0
    min-time: 0
    times: []
  mem-info:
    min-mem: 0
    mem-vals: []
    std-mem: 0
    max-mem: 0
    avg-mem: 0
- location-name: ''
  location-info:
    src-loc-file: ''
    src-loc-start-col: 0
    src-loc-end-line: 0
    src-loc-package: ''
    src-loc-module: ''
    src-loc-end-col: 0
    src-loc-start-line: 0
  tick-info:
    std-ticks: 0
    min-ticks: 0
    max-ticks: 0
    tick-counts: []
    avg-ticks: 0
  time-info:
    max-time: 0
    avg-time: 0
    std-time: 0
    min-time: 0
    times: []
  mem-info:
    min-mem: 0
    mem-vals: []
    std-mem: 0
    max-mem: 0
    avg-mem: 0
- location-name: ''
  location-info:
    src-loc-file: ''
    src-loc-start-col: 0
    src-loc-end-line: 0
    src-loc-package: ''
    src-loc-module: ''
    src-loc-end-col: 0
    src-loc-start-line: 0
  tick-info:
    std-ticks: 0
    min-ticks: 0
    max-ticks: 0
    tick-counts: []
    avg-ticks: 0
  time-info:
    max-time: 0
    avg-time: 0
    std-time: 0
    min-time: 0
    times: []
  mem-info:
    min-mem: 0
    mem-vals: []
    std-mem: 0
    max-mem: 0
    avg-mem: 0
|]



-------------------
-- === Tests === --
-------------------

spec :: Spec
spec = do
    describe "Parsing of Yaml Performance Records" $ do
        it "Succeeds when defaulted"
            $ statsEx1Yaml `shouldDecodeAs` (Just statsEx1)

    describe "Generation of Yaml Performance Records" $ do
        it "Succeeds when defaulted" $ statsEx1 `shouldGenerate` statsEx1Yaml

    describe "Lists of statistics" $ do
        it "Encode successfully" $ statsList `shouldGenerate` statsListYaml
        it "Decode successfully"
            $ statsListYaml `shouldDecodeAs` (Just statsList)

    describe "Computing correct performance statistics" $ do
        it "Succeeds" $ True

    describe "Computing correct statistics differences" $ do
        it "Succeeds" $ True

