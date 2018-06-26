{-# LANGUAGE UndecidableInstances #-}

module Data.Mutable.Class2 where

import Prologue

import qualified Foreign.Storable.Class as Storable
import qualified Memory.Data.Ptr        as Memory
import qualified Type.Known             as Type


-----------------------------
-- === CopyInitializer === --
-----------------------------

class Constructor m a where
    type family Args a
    constructor :: a -> Args a -> m ()

class CopyConstructor m a where
    -- | self -> ref -> action
    copyConstruct :: a -> a -> m ()
