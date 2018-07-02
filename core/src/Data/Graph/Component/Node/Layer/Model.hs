{-# LANGUAGE Strict               #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Component.Node.Layer.Model where

import Prologue

import qualified Data.Construction               as Data
import qualified Data.Graph.Component.Edge.Class as Edge
import qualified Data.Graph.Component.Node.Class as Node
import qualified Data.Graph.Data.Layer.Class     as Layer
import qualified Data.Graph.Data.Layer.Layout    as Layout
import qualified Data.Tag                        as Tag
import qualified Type.Show                       as Type

import Data.Graph.Component.Edge.Class (Edges)
import Data.Graph.Component.Node.Class (Node)
import Data.Graph.Data.Component.List  (ComponentList)
import Data.Graph.Data.Layer.Class     (Layer)



-------------------------
-- === TagOnlyShow === --
-------------------------

-- === Definition === --

data TagOnly = TagOnly
type instance StyledShowOutput TagOnly = Text

-- === API === --

type TagOnlyShow = StyledShow TagOnly

showTag :: TagOnlyShow a => a -> Text
showTag = styledShow TagOnly
{-# INLINE showTag #-}



-------------------
-- === Model === --
-------------------

-- === Definition === --

data Model deriving (Generic)
Tag.family "Tag"


-- === Constructors === --

data family Constructor (tag :: Type) (layout :: Type) :: Type
-- type family Struct      (tag :: Type) ::


-- === Instances === --

instance (name ~ Tag.Instance tag, Type.Show name)
    => StyledShow TagOnly (Constructor tag layout) where
    styledShow _ _ = convert $ Type.show @name

type instance Layout.Merge (Tag a) (Tag b) = TagMerge__ a b
type family TagMerge__ a b where
    TagMerge__ a a = Tag a



-------------------
-- === Model === --
-------------------

-- === Definition === --

instance Data.ShallowDestructor1 IO Node.Uni => Layer Model where
    type Cons Model        = Node.Uni
    type View Model layout = Constructor (Layout.Get Model layout)
    manager = Layer.unsafeOnlyDestructorManager


-- === Utils === --

model :: Layer.ViewReader Node Model layout m
    => Node layout -> m (Layer.ViewData Model layout)
model = Layer.readView @Model
{-# INLINE model #-}




-- === Instances === --

instance (Node.IsUni t, Layer.IsUnwrapped Node.Uni)
      => Layer.IsCons1 Model t where
    cons1 = Node.toUni
    {-# INLINE cons1 #-}
