{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Data.Component.Vector
    (module Data.Graph.Data.Component.Vector, module X) where
import Data.Mutable.Class as X

import Data.Graph.Data.Component.Class
import Prologue                        hiding (FromList, ToList, fromList,
                                        toList)

import qualified Data.Construction            as Data
import qualified Data.Property                as Property
import qualified Data.Vector.Storable.Foreign as Vector
import qualified Foreign.Storable.Utils       as Storable
import qualified Foreign.Storable1.Deriving   as Storable1

import Data.SmallAutoVector.Mutable.Storable (SmallVector)
import Foreign.DynamicStorable               (DynamicStorable)
import Foreign.Storable                      (Storable)


-----------------
-- === Vector === --
-----------------

-- === Definition === --

newtype ComponentVector comp layout
    = ComponentVector (SmallVector 16 (Component comp layout))
    deriving (Eq, Show, Storable) -- Storable, DynamicStorable)
makeLenses       ''ComponentVector
-- Storable1.derive ''ComponentVector


-- -- === API === --

-- fromList :: MonadIO m
--          => [Component comp layout] -> m (ComponentVector comp layout)
-- fromList = \lst -> wrap <$> Vector.fromList lst ; {-# INLINE fromList #-}

-- toList :: MonadIO m => ComponentVector comp layout -> m [Component comp layout]
-- toList = Vector.toList . unwrap ; {-# INLINE toList #-}


-- -- === Instances === --

type instance Item (ComponentVector comp layout) = Component comp layout

instance (FromList m (Unwrapped (ComponentVector comp layout)), Functor m)
      => FromList m (ComponentVector comp layout) where
    fromList = fmap wrap . fromList
    {-# INLINE fromList #-}


instance ToList m (Unwrapped (ComponentVector comp layout))
      => ToList m (ComponentVector comp layout) where
    toList = toList . unwrap
    {-# INLINE toList #-}

-- type instance Property.Get Storable.Dynamics (ComponentVector _)
--    = Storable.Dynamic

instance MonadIO m => Data.ShallowDestructor2 m ComponentVector where
    destructShallow2 = Data.destructShallow1 . unwrap
    {-# INLINE destructShallow2 #-}
