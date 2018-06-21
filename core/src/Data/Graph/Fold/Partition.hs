{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Fold.Partition where

import Prologue

import qualified Data.Graph.Data.Component.Class as Component
import qualified Data.Graph.Data.Component.List  as ComponentList
import qualified Data.Graph.Data.Graph.Class     as Graph
import qualified Data.Graph.Data.Layer.Layout    as Layout
import qualified Data.Graph.Fold.Class           as Fold
import qualified Data.Graph.Fold.Deep            as Deep
import qualified Data.Graph.Fold.Scoped          as Fold
import qualified Data.TypeMap.Strict             as TypeMap

import Data.Graph.Component.Edge       (Target)
import Data.Graph.Component.Node.Layer (Users)
import Data.Graph.Data.Component.Class (Component)
import Data.Graph.Data.Component.List  (ComponentList, ComponentLists)
import Data.TypeMap.Strict             (TypeMap)


----------------------
-- === Clusters === --
----------------------

-- === Definitions === --

type    Clusters__ comps = TypeMap (ComponentLists comps)
newtype Clusters   comps = Clusters (Clusters__ comps)

makeLenses ''Clusters


-- === API === --

splitHead :: TypeMap.SplitHead (ComponentList comp) (ComponentLists comps)
    => Clusters (comp ': comps) -> (ComponentList comp, Clusters comps)
splitHead = fmap wrap . TypeMap.splitHead . unwrap
{-# INLINE splitHead #-}



-------------------------------
-- === Cluster Discovery === --
-------------------------------

-- === Definition === --

type ClustersM  m     = Clusters  (Graph.DiscoverComponents m)
type DiscoveryM m     = Discovery (Graph.DiscoverComponents m)

data Discovery  (comps :: [Type]) deriving (Generic)
type instance Fold.Result     (Discovery comps) = Clusters comps
type instance Fold.LayerScope (Discovery comps)
   = 'Fold.Blacklist '[Target, Users]

type ClusterEditor t ts = TypeMap.ElemEditor (ComponentList  t)
                                             (ComponentLists ts)


-- === API === --

type Partition comp m =
    ( Deep.Builder1 (DiscoveryM m) m (Component comp)
    , Mempty (ClustersM m)
    )

partition :: ∀ comp m layout. Partition comp m
          => Component comp layout -> m (ClustersM m)
partition = Deep.run1 @(DiscoveryM m)
{-# INLINE partition #-}


-- === Instances === --

instance (Monad m, ClusterEditor comp comps)
    => Fold.ComponentBuilder (Discovery comps) m comp where
    componentBuild = \comp acc
       -> wrap
        . TypeMap.modifyElem_ @(ComponentList comp)
          (ComponentList.Cons $ Layout.relayout comp)
        . unwrap
      <$> acc
    {-# INLINE componentBuild #-}


