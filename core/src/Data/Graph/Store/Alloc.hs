{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Store.Alloc where

import Prologue

import qualified Data.Graph.Data.Component.Class as Component
import qualified Data.Graph.Data.Component.List  as ComponentList
import qualified Data.Graph.Fold.Partition       as Partition
import qualified Data.Graph.Store.Size.Discovery as Size
import qualified Data.TypeMap.Strict             as TypeMap
import qualified Foreign.Storable.Class          as Storable
-- import qualified Data.Graph.Store.External       as External
-- import qualified Data.Graph.Store.Size           as Size
-- import qualified Foreign.Info.ByteSize           as ByteSize

-- import Data.Graph.Data.Component.Class (Component)
import Data.Graph.Data.Component.List (ComponentList, ComponentLists)
import Data.Graph.Store.Size.Class    (DynamicSize, Size (Size))
-- import Data.Graph.Store.MemoryRegion   (MemoryRegion (MemoryRegion))
-- import Data.Graph.Store.Size           (Size)
-- import Foreign.ForeignPtr.Utils        (mallocForeignPtrBytes, plusForeignPtr)


------------------
-- === Size === --
------------------

-- === Definition === --

-- data Size = Size
--     { _staticSize   :: Int
--     , _externalSize :: External.Size
--     } deriving Show
-- makeLenses ''Size


-- -- === Instances === --

-- instance Mempty Size where
--     mempty = Size 0 mempty
--     {-# INLINE mempty #-}

-- instance Semigroup Size where
--     (<>) = \(Size s1 d1) (Size s2 d2) ->
--         let s = s1 + s2
--             d = d1 <> d2
--         in Size s d
--     {-# INLINE (<>) #-}


-- === Helpers === --

-- totalSize :: Size -> Int
-- totalSize (Size static external) = static + (External.totalSize external)
-- {-# INLINE totalSize #-}

-- allocRegionForSize :: MonadIO m => Size -> m MemoryRegion
-- allocRegionForSize = \size -> do
--     ptr <- liftIO $ mallocForeignPtrBytes $! Size.total size
--     let dynSize    = size ^. Size.dynamic . Size.dataRegion
--         dynMemPtr  = plusForeignPtr ptr $! size ^. staticSize
--         ptrsMemPtr = plusForeignPtr dynMemPtr dynSize
--     pure $! MemoryRegion ptr dynMemPtr ptrsMemPtr
-- {-# INLINE allocRegionForSize #-}


-- -----------------------
-- -- === Discovery === --
-- -----------------------

-- -- === Component === --

-- type ComponentSizeDiscovery comp m =
--     ( External.SizeDiscovery1 m (Component comp)
--     , ByteSize.Known (Component comp) m
--     )

-- componentSize :: ∀ comp m. ComponentSizeDiscovery comp m
--     => Component.Some comp -> m Size
-- componentSize = \comp -> do
--     staticSize   <- ByteSize.get @(Component comp)
--     externalSize <- External.size comp
--     pure $! Size staticSize externalSize
-- {-# INLINE componentSize #-}


-- === Cluster size discovery === --


--------------------------
-- === Cluster size === --
--------------------------

-- === API === --

type ClusterSize comps m =
    ( ClusterStaticSizeGetter comps
    , ClusterDynamicSizeFold  comps m
    , Functor m
    )

clusterSize :: ∀ comps m. ClusterSize comps m
    => Partition.Clusters comps -> m Size
clusterSize = \cluster -> do
    let staticSize = clusterStaticSize @comps cluster
    dynamicSize <- foldClusterDynamicSize @comps cluster mempty
    pure $ Size staticSize dynamicSize
{-# INLINE clusterSize #-}


-- === ClusterStaticSizeGetter === --

class ClusterStaticSizeGetter comps where
    clusterStaticSize :: Partition.Clusters comps -> Int

instance ClusterStaticSizeGetter '[] where
    clusterStaticSize = const 0
    {-# INLINE clusterStaticSize #-}

instance
    ( TypeMap.SplitHead (ComponentList comp) (ComponentLists comps)
    , Storable.KnownConstantStaticSize comp
    , ClusterStaticSizeGetter comps
    ) => ClusterStaticSizeGetter (comp ': comps) where
    clusterStaticSize = \cluster ->
        let (  !compList
             , !cluster' ) = TypeMap.splitHead cluster
            compSize       = Storable.constantStaticSize @comp
            compsSize      = compSize * ComponentList.length compList
            totalSize      = compsSize + clusterStaticSize @comps cluster'
        in  totalSize
    {-# INLINE clusterStaticSize #-}


-- === ClusterDynamicSizeFold === --

class ClusterDynamicSizeFold comps m where
    foldClusterDynamicSize :: Partition.Clusters comps
                           -> DynamicSize -> m DynamicSize

instance Applicative m
     => ClusterDynamicSizeFold '[] m where
    foldClusterDynamicSize = const pure
    {-# INLINE foldClusterDynamicSize #-}

instance
    ( TypeMap.SplitHead (ComponentList comp) (ComponentLists comps)
    , Size.DynamicDiscovery m (Component.Some comp)
    , ClusterDynamicSizeFold comps m
    , Monad m
    ) => ClusterDynamicSizeFold (comp ': comps) m where
    foldClusterDynamicSize cluster acc = do
        let (!compList, !cluster') = TypeMap.splitHead cluster
            sizeAcc  = \acc -> fmap (acc <>) . Size.discoverDynamic
        listSize <- ComponentList.foldlM sizeAcc mempty compList
        foldClusterDynamicSize @comps cluster' $! acc <> listSize
    {-# INLINE foldClusterDynamicSize #-}



-- -------------------------------
-- -- === Memory allocation === --
-- -------------------------------

-- type Allocator comps m =
--     ( ClusterSizeDiscovery comps m
--     , MonadIO m
--     )

-- alloc :: ∀ comps m. Allocator comps m
--       => Partition.Clusters comps -> m MemoryRegion
-- alloc = allocRegionForSize <=< clusterSize @comps
-- {-# INLINE alloc #-}
