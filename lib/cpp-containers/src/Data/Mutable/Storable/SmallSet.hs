{-# LANGUAGE UndecidableInstances #-}

module Data.Mutable.Storable.SmallSet
    (module Data.Mutable.Storable.SmallSet, module X) where
import Data.Mutable.Class as X

import Prologue hiding (Read, length, unsafeRead)

import qualified Data.SmallAutoVector.Mutable.Storable as SmallVector
import qualified Data.Storable                         as Struct
import qualified Foreign.Storable.Class                as Storable

import Data.SmallAutoVector.Mutable.Storable (MemChunk, SmallVector)
import Foreign.Storable.Class                (Copy, Storable, View)



----------------------
-- === SmallSet === --
----------------------

-- === Definition === --

type    SetImp__ (n :: Nat) a = SmallVector n a
newtype SmallSet (n :: Nat) a = SmallSet (SetImp__ n a)
    deriving (Eq, Ord, NFData)
makeLenses ''SmallSet

type instance Item (SmallSet n a) = a


-- === Utils === --

type LookupAndApp m n a = (MonadIO m, Ord a, Read m (SmallSet n a))

lookupAndApp :: LookupAndApp m n a
    => (Int -> m ()) -> (Int -> m ()) -> SmallSet n a -> a -> m ()
lookupAndApp = \ffound fmissing s a -> do
    siz <- size s
    if siz == 0 then fmissing 0
    else let min = 0
             max = siz - 1
             ix  = max `quot` 2
         in  lookupAndApp__ ffound fmissing s a min ix max
{-# INLINE lookupAndApp #-}

lookupAndApp__ :: LookupAndApp m n a
    => (Int -> m ()) -> (Int -> m ())
    -> SmallSet n a -> a -> Int -> Int -> Int -> m ()
lookupAndApp__ ffound fmissing s a = go where
    go = \min ix max -> do
        ixVal <- unsafeRead s ix
        if max <= min then
             if      a < ixVal then fmissing ix
             else if a > ixVal then fmissing (ix + 1)
             else                   ffound   ix
        else if a < ixVal then
                 let max' = ix - 1
                     ix'  = (min + max') `quot` 2
                 in  go min ix' max'
             else if a > ixVal then
                 let min' = ix + 1
                     ix'  = (min' + max) `quot` 2
                 in  go min' ix' max
             else ffound ix
    {-# INLINABLE go #-}
{-# INLINE lookupAndApp__ #-}


-- === API Instances === --

instance (PlacementNew m (SetImp__ n a), Functor m)
      => PlacementNew m (SmallSet n a) where
    placementNew = fmap wrap . placementNew . coerce
    {-# INLINE placementNew #-}

instance (New m (SetImp__ n a), Functor m)
      => New m (SmallSet n a) where
    new = wrap <$> new
    {-# INLINE new #-}

instance Size m (SetImp__ n a)
      => Size m (SmallSet n a) where
    size = size . unwrap
    {-# INLINE size #-}

instance Capacity m (SetImp__ n a)
      => Capacity m (SmallSet n a) where
    capacity = capacity . unwrap
    {-# INLINE capacity #-}

instance Read m (SetImp__ n a)
      => Read m (SmallSet n a) where
    unsafeRead = unsafeRead . unwrap
    {-# INLINE unsafeRead #-}

instance (InsertAt m (SetImp__ n a), LookupAndApp m n a)
      => Insert m (SmallSet n a) where
    insert = \a v -> lookupAndApp (\_ -> pure ())
                     (\ix -> insertAt (unwrap a) ix v) a v
    {-# INLINE insert #-}

instance (RemoveAt m (SetImp__ n a), LookupAndApp m n a)
      => Remove m (SmallSet n a) where
    remove = \a v -> lookupAndApp (removeAt (unwrap a))
                     (\_ -> pure ()) a v
    {-# INLINE remove #-}


-- === Debug Instances === --

instance Show (SetImp__ n a)
      => Show (SmallSet n a) where
    show = show . unwrap
