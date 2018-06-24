{-# LANGUAGE UndecidableInstances #-}

module Data.Mutable.Storable.SmallSet
    (module Data.Mutable.Storable.SmallSet, module X) where
import Data.Mutable.Class as X

import Prologue hiding (FromList, Read, ToList, fromList, length, toList,
                 unsafeRead)

import qualified Data.Construction                     as Data
import qualified Data.Mutable.Storable.SmallAutoVector as SmallVector
import qualified Data.Storable                         as Struct
import qualified Foreign.Storable                      as StdStorable
import qualified Foreign.Storable.Class                as Storable
import qualified Type.Known                            as Type

import Data.Mutable.Storable.SmallAutoVector (MemChunk, SmallVector)
import Foreign.Storable.Class                (Copy, Storable, View)
import System.IO.Unsafe                      (unsafeDupablePerformIO)



----------------------
-- === SmallSet === --
----------------------

-- === Definition === --

type SmallSet__ = SmallVector
newtype SmallSet (n :: Nat) a = SmallSet (SmallSet__ n a)
    deriving (Eq, Ord, NFData, Free m)
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

instance Storable.KnownConstantSize (SmallSet__ n a)
      => Storable.KnownConstantSize (SmallSet n a) where
    constantSize = Storable.constantSize @(SmallSet__ n a)
    {-# INLINABLE constantSize #-}

instance Storable.KnownSize t m (SmallSet__ n a)
      => Storable.KnownSize t m (SmallSet n a) where
    size = Storable.size @t . unwrap
    {-# INLINE size #-}

instance (PlacementNew m (SmallSet__ n a), Functor m)
      => PlacementNew m (SmallSet n a) where
    placementNew = fmap wrap . placementNew . coerce
    {-# INLINE placementNew #-}

instance (New m (SmallSet__ n a), Functor m)
      => New m (SmallSet n a) where
    new = wrap <$> new
    {-# INLINE new #-}

instance Size m (SmallSet__ n a)
      => Size m (SmallSet n a) where
    size = size . unwrap
    {-# INLINE size #-}

instance Capacity m (SmallSet__ n a)
      => Capacity m (SmallSet n a) where
    capacity = capacity . unwrap
    {-# INLINE capacity #-}

instance Read m (SmallSet__ n a)
      => Read m (SmallSet n a) where
    unsafeRead = unsafeRead . unwrap
    {-# INLINE unsafeRead #-}

instance (InsertAt m (SmallSet__ n a), LookupAndApp m n a)
      => Insert m (SmallSet n a) where
    insert = \a v -> lookupAndApp (\_ -> pure ())
                     (\ix -> insertAt (unwrap a) ix v) a v
    {-# INLINE insert #-}

instance (RemoveAt m (SmallSet__ n a), LookupAndApp m n a)
      => Remove m (SmallSet n a) where
    remove = \a v -> lookupAndApp (removeAt (unwrap a))
                     (\_ -> pure ()) a v
    {-# INLINE remove #-}

instance (FromList m (SmallSet__ n a), Functor m)
      => FromList m (SmallSet n a) where
    fromList = fmap wrap . fromList
    {-# INLINE fromList #-}

instance ToList m (SmallSet__ n a)
      => ToList m (SmallSet n a) where
    toList = toList . unwrap
    {-# INLINE toList #-}



-- === Debug Instances === --

instance Show (SmallSet__ n a)
      => Show (SmallSet n a) where
    show = show . unwrap


-- === Deprecated Instances === --

deriving instance StdStorable.Storable (SmallSet__ n a)
    => StdStorable.Storable (SmallSet n a)

-- WARNING: this instance is strange. It does not release self-memory,
--          because it is used for placement-new objects
instance (Data.Destructor1 m (SmallSet__ n), Monad m)
      => Data.Destructor1 m (SmallSet n) where
    destruct1 = Data.destruct1 . unwrap
    {-# INLINE destruct1 #-}
