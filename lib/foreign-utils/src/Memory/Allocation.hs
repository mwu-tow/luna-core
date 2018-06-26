{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Memory.Allocation where

import Prologue

import qualified Foreign.Storable.Class as Storable
import qualified Memory.Data.Ptr        as Memory


-------------------------------
-- === Memory Allocation === --
-------------------------------

-- === Definition === --

data Allocator = Allocator Type

class Allocation (alloc :: Allocator) a m where
    allocate :: ∀ t. Memory.PtrType t => Int -> m (Memory.Ptr t a)


-- === StdAllocator === --

data Std
type StdAllocator = 'Allocator Std
instance (MonadIO m, Storable.KnownConstantSize a)
      => Allocation StdAllocator a m where
    allocate = Memory.mallocBytes . (Storable.constantSize @a *)
    {-# INLINE allocate #-}



------------------------------------
-- === Allocator manipulation === --
------------------------------------

-- === Definition === --

type family GetAllocator (t :: k) :: Allocator where
    GetAllocator (t a) = a
    GetAllocator (t _) = GetAllocator t
    GetAllocator _     = StdAllocator

type family SetAllocator (alloc :: Allocator) (t :: k) :: k where
    SetAllocator a (t _) = a
    SetAllocator a (t s) = SetAllocator a t s


-- === Utils === --

setAllocator :: ∀ alloc a. a -> SetAllocator alloc a
setAllocator = unsafeCoerce
{-# INLINE setAllocator #-}
