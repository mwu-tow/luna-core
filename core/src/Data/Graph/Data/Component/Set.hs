{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Data.Component.Set
    (module Data.Graph.Data.Component.Set, module X) where
import Data.Mutable.Class as X

import Prologue hiding (FromList, ToList, fromList, toList)

import qualified Data.Construction          as Data
import qualified Data.Mutable.Plain         as Data
import qualified Data.Property              as Property
import qualified Foreign.Storable.Class     as Storable
import qualified Foreign.Storable1.Deriving as Storable1

import Data.Graph.Data.Component.Class (Component)
import Data.Mutable.Storable.SmallSet  (SmallSet)
-- import Foreign.DynamicStorable (DynamicStorable)
import Foreign.Storable (Storable)



--------------------------
-- === ComponentSet === --
-------------------------------------------

-- === Definition === --

type    ComponentSet__ tag layout = SmallSet 0 (Component tag layout)
newtype ComponentSet   tag layout = ComponentSet (ComponentSet__ tag layout)
    deriving (Show, Storable, Remove m, ToList m, Size m)


-- === Instances === --

deriving instance Insert m (ComponentSet__ tag layout)
               => Insert m (ComponentSet tag layout)

deriving instance Data.CopyInitializer m (ComponentSet__ tag layout)
               => Data.CopyInitializer m (ComponentSet tag layout)

instance Data.CopyInitializer  m (ComponentSet tag ())
      => Data.CopyInitializer1 m (ComponentSet tag) where
    copyInitialize1 = \a -> Data.copyInitialize (coerce a :: ComponentSet tag ())
    {-# INLINE copyInitialize1 #-}

-- type instance Property.Get Storable.Dynamics (ComponentSet _) = Storable.Dynamic

type instance Item (ComponentSet tag layout) = Component tag layout

instance Storable.KnownConstantSize (ComponentSet__ n a)
      => Storable.KnownConstantSize (ComponentSet n a) where
    constantSize = Storable.constantSize @(ComponentSet__ n a)
    {-# INLINE constantSize #-}

instance Storable.KnownSize t m (ComponentSet__ n a)
      => Storable.KnownSize t m (ComponentSet n a) where
    size = Storable.size @t . unwrap
    {-# INLINE size #-}

instance (FromList m (Unwrapped (ComponentSet comp layout)), Functor m)
      => FromList m (ComponentSet comp layout) where
    fromList = fmap wrap . fromList
    {-# INLINE fromList #-}

instance (New m (Unwrapped (ComponentSet comp layout)), Functor m)
      => New m (ComponentSet comp layout) where
    new = wrap <$> new
    {-# INLINE new #-}

-- instance ToList m (Unwrapped (ComponentSet comp layout))
--       => ToList m (ComponentSet comp layout) where
--     toList = toList . unwrap
--     {-# INLINE toList #-}

-- instance MonadIO m => Set.Set m (ComponentSet tag layout) where
--     new    = wrap <$> Set.new    ; {-# INLINE new    #-}
--     insert = Set.insert . unwrap ; {-# INLINE insert #-}
--     delete = Set.delete . unwrap ; {-# INLINE delete #-}
--     member = Set.member . unwrap ; {-# INLINE member #-}
--     size   = Set.size   . unwrap ; {-# INLINE size   #-}
--     null   = Set.null   . unwrap ; {-# INLINE null   #-}
--     toList = Set.toList . unwrap ; {-# INLINE toList #-}

-- instance MonadIO m => Data.Constructor2 m () ComponentSet where
--     construct2 = \ _ -> wrap <$> Data.construct1'
--     {-# INLINE construct2 #-}

instance MonadIO m => Data.ShallowDestructor2 m ComponentSet where
    destructShallow2 = Data.destruct1 . unwrap
    {-# INLINE destructShallow2 #-}

instance MonadIO m
      => Storable.KnownSize2 Storable.Dynamic m ComponentSet where
    size2 = Storable.size @Storable.Dynamic . unwrap
    {-# INLINE size2 #-}

makeLenses ''ComponentSet
Storable1.derive ''ComponentSet
