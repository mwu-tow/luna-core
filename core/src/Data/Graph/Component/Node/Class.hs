{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Component.Node.Class where

import Prologue

import qualified Data.Graph.Data              as Component
import qualified Data.Graph.Data.Layer.Layout as Layout
import qualified Data.Tag                     as Tag

import Data.Generics.Traversable (GTraversable)


------------------
-- === Node === --
------------------

-- === Definition === --

data Nodes deriving (Generic)
type Node = Component.Component Nodes
type Some = Component.Some      Nodes


---------------------
-- === UniTerm === --
---------------------

-- | The implementation of Uni is delayed until we know
--   all possible Term constructors.
type family Uni :: Type -> Type

class IsUni t where
    toUni :: ∀ a. t a -> Uni a



-- === Discovery === --

-- | IsTermTag is used to gather all IR terms when generating UniNode in TH.
class IsTermTag (t :: Type)
