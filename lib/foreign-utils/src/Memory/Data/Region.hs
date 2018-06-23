{-# LANGUAGE UndecidableInstances #-}

module Memory.Data.Region where

import Prologue

import qualified Foreign.Storable.Class as Storable
import qualified Memory.Data.Ptr        as Memory 



----------------------------
-- === ConstantRegion === --
----------------------------

-- === Definition === --

newtype ConstantRegion (n :: Nat) t a = ConstantRegion (Memory.Ptr t a)
makeLenses ''ConstantRegion


-- === Instances === --

type instance Storable.ConstantSize Storable.Static (ConstantRegion n a)
            = Storable.ConstantSize Storable.Static a * n

-- instance Applicative m
--       => Storable.Peek Struct.Field m (ConstantRegion n a) where
--     peek = pure . coerce
--     {-# INLINE peek #-}




-----------------------------
-- === DynamicMemChunk === --
-----------------------------

-- === Definition === --

newtype UnknownSizeRegion t a = UnknownSizeRegion (Memory.Ptr t a)
makeLenses ''UnknownSizeRegion

unsafeNull :: Memory.NullPtr t => UnknownSizeRegion t a
unsafeNull = UnknownSizeRegion Memory.nullPtr
{-# INLINE unsafeNull #-}

-- -- FIXME: change it to Struct.FieldInitializer
-- instance Applicative m
--       => Storable.Poke Struct.Field m (UnknownSizeRegion t a) where
--     poke = \_ _ -> pure ()
--     {-# INLINE poke #-}