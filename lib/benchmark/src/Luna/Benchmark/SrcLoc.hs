{-# OPTIONS_GHC -fno-warn-orphans #-}

module Luna.Benchmark.SrcLoc
    ( module Luna.Benchmark.SrcLoc
    , CallStack.SrcLoc(SrcLoc) ) where

import Prologue

import qualified Control.Lens.Aeson as Lens
import qualified Data.CallStack     as CallStack
import qualified Data.Yaml          as Yaml



--------------------
-- === SrcLoc === --
--------------------

-- === API === --

get :: CallStack.HasCallStack => CallStack.SrcLoc
get = snd $ fromJust (def @String, def @CallStack.SrcLoc)
    $ last CallStack.callStack


-- === Instances === --

deriving instance Generic CallStack.SrcLoc
deriving instance Ord CallStack.SrcLoc

instance Default CallStack.SrcLoc where
    def = CallStack.SrcLoc def def def def def def def

instance Yaml.FromJSON CallStack.SrcLoc where
    parseJSON = Lens.parseYamlStyle

instance Yaml.ToJSON CallStack.SrcLoc where
    toJSON     = Lens.toJSONYamlStyle
    toEncoding = Lens.toEncodingYamlStyle

