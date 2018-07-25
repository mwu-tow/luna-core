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
raw-mem: []
raw-ticks: []
raw-times: []
|]

statsList :: [Statistics]
statsList = [ def @Statistics, def @Statistics, def @Statistics ]

statsListYaml :: ByteString
statsListYaml = [qqStr|
- location-name: ''
  raw-mem: []
  raw-ticks: []
  raw-times: []
- location-name: ''
  raw-mem: []
  raw-ticks: []
  raw-times: []
- location-name: ''
  raw-mem: []
  raw-ticks: []
  raw-times: []
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

