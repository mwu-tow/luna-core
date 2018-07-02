module Data.Tag.Class where

import Data.Kind
import Prologue


-- === Definitions === --

data Tag (fam :: *) (inst :: *) deriving (Generic)
type family TagOf a :: Type


-- === API === --

type family Family   tag where Family   (Tag fam _   ) = fam
type family Instance tag where Instance (Tag _   inst) = inst
