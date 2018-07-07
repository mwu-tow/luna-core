{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph2.Class where

import Prologue

import qualified Data.Storable2              as Struct
import qualified Data.Vector.Unboxed.Mutable as Vector
import qualified Memory                      as Memory

import Data.Vector.Unboxed.Mutable (MVector)





type family Spec (t :: Type) (k :: Type) :: f


-----------------------
-- === Component === --
-----------------------

-- === Definition === --

data Component (tag :: Type) (t :: Type)
  --  = Component (Struct )

class IsComponent t where
    type family Tag t
    type family Tp  t
    component :: Iso' t (Component (Tag t) (Tp t))

    type Tag t = Tag (Unwrapped t)
    type Tp  t = Tp  (Unwrapped t)
    default component ::
        ( Tag t ~ Tag (Unwrapped t)
        , Tp  t ~ Tp  (Unwrapped t)
        , Wrapped t
        , IsComponent (Unwrapped t)
        ) => Iso' t (Component (Tag t) (Tp t))
    component = wrapped' . component
    {-# INLINE component #-}

instance IsComponent (Component tag t) where
    type Tag (Component tag t) = tag
    type Tp  (Component tag t) = t
    component = id
    {-# INLINE component #-}



------------------
-- === Node === --
------------------

data Nodes
type Node = Component Nodes


------------------
-- === Edge === --
------------------

data Edges
type Edge = Component Edges




-- alloc :: ∀ tag t m. m (Component tag t)
-- alloc = undefined -- \_ -> Memory.allocate @(Spec t Memory.Allocator) 0


class Monad m => Allocator tag t m where
    alloc__ :: m (Component tag t)

alloc :: (Allocator (Tag a) (Tp a) m, IsComponent a) => m a
alloc = view (from component) <$> alloc__


newtype Term t layout = Term (Node t)
makeLenses ''Term

instance IsComponent (Term t layout)


foo :: Allocator Nodes t m => m (Term t layout)
foo = alloc
