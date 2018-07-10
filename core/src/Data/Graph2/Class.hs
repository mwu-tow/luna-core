{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph2.Class where

import Prologue hiding (ToList)

import qualified Data.Storable2              as Struct
import qualified Data.Vector.Unboxed.Mutable as Vector
import qualified Memory                      as Memory

import Data.Storable2              (Struct)
import Data.Vector.Unboxed.Mutable (MVector)





type family ToList (a :: k) :: [Type]

data Inputs  a = Inputs  [Type]
data Outputs a = Outputs [Type]





----------------------
-- === Property === --
----------------------

-- === Definition === --

type family DataKind (key :: Type) :: Type

type family Get key obj :: Type -- DataKind key -- GHC Bug until 8.6


type family MapGet key objs where
    MapGet _ '[] = '[]
    MapGet k (a ': as) = Get k a ': MapGet k as



---------------------------
-- === Specification === --
---------------------------

type family Spec (tgt :: Type) (key :: Type) :: key



-------------------------------
-- === Graph Description === --
-------------------------------

-- === Definition === --

data LayerData
data StaticLayers  = StaticLayers  [Type]
data DynamicLayers = DynamicLayers [Type]


-- === Utils === --

type GetStaticLayers  g = MapGet LayerData (ToList (Spec g StaticLayers))
type GetDynamicLayers g = MapGet LayerData (ToList (Spec g DynamicLayers))


-- === Instances === --

type instance ToList ('StaticLayers  lst) = lst
type instance ToList ('DynamicLayers lst) = lst










-----------------------------
-- === ComponentLayout === --
-----------------------------

-- === Definition === --

data ComponentLayout (graph :: Type) (tag :: Type)


-- === Instances === --

type instance Struct.Fields (ComponentLayout graph tag) = GetStaticLayers graph
type instance Struct.FieldType layer (ComponentLayout graph tag)
            = Get LayerData layer




-----------------------
-- === Component === --
-----------------------

-- === Definition === --

newtype Component (graph :: Type) (tag :: Type) = Component
    (Struct (Spec graph Memory.ManagementType) (ComponentLayout graph tag))
makeLenses ''Component


-- === IsComponent === --

class IsComponent a where
    type family Graph a
    type family Tag   a
    component :: Iso' a (Component (Graph a) (Tag a))

    type Graph a = Graph (Unwrapped a)
    type Tag   a = Tag   (Unwrapped a)
    default component ::
        ( Graph a ~ Graph (Unwrapped a)
        , Tag   a ~ Tag   (Unwrapped a)
        , Wrapped a
        , IsComponent (Unwrapped a)
        ) => Iso' a (Component (Graph a) (Tag a))
    component = wrapped' . component
    {-# INLINE component #-}

instance IsComponent (Component graph tag) where
    type Graph (Component graph tag) = graph
    type Tag   (Component graph tag) = tag
    component = id
    {-# INLINE component #-}


-- === Instances === --

instance Struct.IsStruct (Component grah tag)
type instance Memory.Management (Component graph tag)
            = Memory.Management (Unwrapped (Component graph tag))



------------------
-- === Node === --
------------------

data Nodes
type Node graph = Component graph Nodes


------------------
-- === Edge === --
------------------

data Edges
type Edge graph = Component graph Edges





-- alloc2 :: ∀ tag t m. m (Component tag t)
-- alloc2 = undefined -- \_ -> Memory.allocate @(Spec t Memory.Allocator) 0


class Monad m => Allocator graph tag m where
    alloc__ :: m (Component graph tag)


-- instance Allocator tag t m where
--     alloc__ = Memory.allocate @(Spec )




alloc :: (Allocator (Graph a) (Tag a) m, IsComponent a) => m a
alloc = view (from component) <$> alloc__


newtype IRComponent graph tag layout = IRComponent (Component graph tag)
makeLenses ''IRComponent

type Term graph = IRComponent graph Nodes
type Link graph = IRComponent graph Edges


instance IsComponent (IRComponent graph tag layout)


-- foo :: Allocator graph Nodes m => m (Term graph layout)
-- foo = alloc




data Layouted (t :: Type -> Type)

data X

data UniTerm (layout :: Type)
type SomeUniTerm = UniTerm ()

data Model

-- LayerData is graph abstraction, not IR abstraction, so it does not know about layout

-- type family LayerData layer
-- type family MapLayerData layers where
--     MapLayerData '[]       = '[]
--     MapLayerData (l ': ls) = LayerData l ': MapLayerData ls


type instance Get LayerData Model = Layouted UniTerm


-- type family ValKind (t :: Type) :: Type

type instance Spec X key = SpecX key
type family SpecX key :: key where
    SpecX Memory.Allocator = Memory.StdAllocator
    SpecX StaticLayers     = 'StaticLayers '[Model]
    -- SpecX (Inputs A)       = 'Inputs '[Int]
    -- SpecX (Inputs B)       = 'Inputs '[String]
    -- SpecX Int = Int


type family ApplyLayout layout t where
    ApplyLayout layout (Layouted t) = t layout
    ApplyLayout layout t            = t

-- maybe it should be named GraphReader and Reader shuld discover graph type?
class Reader graph tag layer m where
    read :: Component graph tag -> m (Get LayerData layer)

instance Struct.Reader2 layer (Component graph tag)
      => Reader graph tag layer m where
    read = Struct.read2' @layer
    -- TODO: finish read2x implementations and care about infered constraints

-- read :: ∀ layer graph tag m. Struct.Reader2 layer (Component graph tag)
    -- => Component graph tag -> m (LayerData layer)
-- readFieldLayer = \(Component struct) -> Struct.read2 (Struct.autoLens :: Struct.Lens Model) struct


-- readLayer :: () => IRComponent graph tag layout -> m



-- type family ValKind t where
--     ValKind (t :: Type)

-- type instance ValKind Memory.Allocator = Memory.Allocator
-- type instance ValKind Type = Type





