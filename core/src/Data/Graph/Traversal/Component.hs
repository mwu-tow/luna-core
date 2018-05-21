-- {-# LANGUAGE Strict               #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Traversal.Component where

import Prologue hiding (Foldable, Foldable1, Traversable, fold, fold1, traverse)

import qualified Control.Monad.State.Layered          as State
import qualified Data.Generics.Traversable            as GTraversable
import qualified Data.Graph.Component.Edge            as Edge
import qualified Data.Graph.Component.Node.Class      as Node
import qualified Data.Graph.Component.Node.Layer.Type as Type
import qualified Data.Graph.Data.Component.Class      as Component
import qualified Data.Graph.Data.Component.Set        as Component
import qualified Data.Graph.Data.Graph.Class          as Graph
import qualified Data.Graph.Data.Layer.Class          as Layer
import qualified Data.Graph.Data.Layer.Layout         as Layout
import qualified Data.Graph.Traversal.Fold            as Fold
import qualified Data.Map.Strict                      as Map
import qualified Data.PtrSet.Mutable                  as PtrSet
import qualified Data.Set                             as Set
import qualified Foreign.Ptr                          as Ptr
import qualified Foreign.Storable                     as Storable
import qualified Type.Data.List                       as List

import Data.Generics.Traversable             (GTraversable)
import Data.Graph.Component.Node.Layer.Model (Model)
import Data.Graph.Data.Component.Class       (Component)
import Data.PtrList.Mutable                  (UnmanagedPtrList)
import Data.Set                              (Set)
import Data.Vector.Storable.Foreign          (Vector)
import Foreign.Ptr.Utils                     (SomePtr)
import Type.Data.Bool                        (Not, type (||))

import Data.Graph.Component.Node.Class (Constructor)


--------------------------------
-- === ComponentDiscovery === --
--------------------------------

-- === Definition === --

data ComponentDiscovery comp
type instance Fold.Result     (ComponentDiscovery comp) = [Component.Some comp]
type instance Fold.LayerScope (ComponentDiscovery comp) = 'Fold.All


-- === API === --

discoverComponents :: ∀ comp a m. Fold.Foldable (ComponentDiscovery comp) m a
    => a -> m [Component.Some comp]
discoverComponents = \a -> Fold.buildFold @(ComponentDiscovery comp) a $! pure $! mempty ; {-# INLINE discoverComponents #-}


-- === Instances === --

instance Monad m => Fold.FoldableComponent (ComponentDiscovery comp) m tag where
    buildComponentFold = \_ a -> a ; {-# INLINE buildComponentFold #-}


instance {-# OVERLAPPABLE #-} Monad m
      => Fold.Foldable1 (ComponentDiscovery comp) m (Component comp') where
    buildFold1 = \_ a -> a ; {-# INLINE buildFold1 #-}

instance Monad m
      => Fold.Foldable1 (ComponentDiscovery comp) m (Component comp) where
    buildFold1 = \comp mr -> (Layout.relayout comp :) <$> mr
    {-# INLINE buildFold1 #-}

instance {-# OVERLAPPABLE #-} Monad m
      => Fold.Foldable (ComponentDiscovery comp) m (Component comp' l) where
    buildFold = \_ a -> a ; {-# INLINE buildFold #-}

instance Monad m
      => Fold.Foldable (ComponentDiscovery comp) m (Component comp l) where
    buildFold = \comp mr -> (Layout.relayout comp :) <$> mr
    {-# INLINE buildFold #-}

instance {-# OVERLAPPABLE #-} Monad m
      => Fold.Foldable1 (ComponentDiscovery comp) m (Component.Set comp') where
    buildFold1 = \_ -> id ; {-# INLINE buildFold1 #-}

instance MonadIO m
      => Fold.Foldable1 (ComponentDiscovery comp) m (Component.Set comp) where
    buildFold1 = \a acc -> (\a b -> a <> b) <$> (Layout.relayout <<$>> PtrSet.toList (unwrap a))
                                            <*> acc
    {-# INLINE buildFold1 #-}


instance (MonadIO m, Show (Constructor t a), GTraversable (Fold.Foldable (ComponentDiscovery comp) m) (Constructor t a))
    => Fold.Foldable (ComponentDiscovery comp) m (Constructor t a) where
    buildFold a mx = do
        print ">>>"
        print a
        out <- Fold.gbuildFold @(ComponentDiscovery comp) a mx
        print out
        print "<<<"
        pure out
    {-# INLINE buildFold #-}

instance Monad m => Fold.Foldable s m (UnmanagedPtrList x) where
    buildFold = undefined

instance Monad m => Fold.Foldable s m (Vector x) where
    buildFold = undefined
