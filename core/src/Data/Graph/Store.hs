module Data.Graph.Store where

import Prologue

import qualified Data.Graph.Data.Graph.Class     as Graph
import qualified Data.Graph.Fold.Partition       as Partition
import qualified Data.Graph.Store.Alloc          as Alloc
import qualified Data.Graph.Store.Buffer         as Buffer
import qualified Data.Graph.Store.Internal       as Serialize
import qualified Data.Graph.Store.Size.Discovery as Size
import qualified Memory                          as Memory

import Data.Graph.Data.Component.Class (Component)
-- import Data.Graph.Store.MemoryRegion   (MemoryRegion)


-- -----------------------
-- -- === Discovery === --
-- -----------------------

-- -- === Definition === --

-- data Dump (scope :: Fold.Scope)
-- type instance Fold.Result     (Dump scope) = Component.List'
-- type instance Fold.LayerScope (Dump scope) = scope


-- -- === API === --

-- type Dumpable  scope = Deep.Builder  (Dump scope)
-- type Dumpable1 scope = Deep.Builder1 (Dump scope)

-- dump  :: ∀ scope m a.   Dumpable  scope m a => a   -> m Component.List'
-- dump1 :: ∀ scope m a t. Dumpable1 scope m a => a t -> m Component.List'
-- dump  = Deep.run  @(Dump scope)
-- dump1 = Deep.run1 @(Dump scope)
-- {-# INLINE dump #-}
-- {-# INLINE dump1 #-}


-- -- === Simple === --

-- -- type SimpleDiscoveryScope = 'Fold.Whitelist '[Model, Type.Type, Source]
-- -- type Dumpable'   = Dumpable  SimpleDiscoveryScope
-- -- type Dumpable1'  = Dumpable1 SimpleDiscoveryScope

-- -- dump'  :: ∀ m a.   Dumpable'  m a => a   -> m Component.List'
-- -- dump1' :: ∀ m a t. Dumpable1' m a => a t -> m Component.List'
-- -- dump'  = dump  @SimpleDiscoveryScope
-- -- dump1' = dump1 @SimpleDiscoveryScope
-- -- {-# INLINE dump'  #-}
-- -- {-# INLINE dump1' #-}


-- -- === Instances === --

-- instance Monad m => Fold.LayerBuilder (Dump scope) m layer where
--     layerBuild = \layer macc -> do

--         (Component.Cons $ Layout.relayout cmp) <$> acc
--     {-# INLINE componentBuild #-}



------------------------------------
-- === SubGraph serialization === --
------------------------------------

type Serializer comp m =
    ( MonadIO m
    , Partition.Partition comp m
    , Size.ClusterSizeDiscovery (Graph.ComponentsM m) m
    , Size.ClusterSizeCount     (Graph.ComponentsM m) m
    , Buffer.Alloc m

    , Buffer.StaticRegionEncoder (Graph.ComponentsM m) m
    , Buffer.CompsCopyInitializationX (Graph.ComponentsM m) m

    , Show (Partition.ClustersM m)
    -- , Alloc.Allocator comps m
    -- , Serialize.ClusterSerializer comps m
    )

serialize :: ∀ comp m layout. Serializer comp m
    => Component comp layout -> m () -- MemoryRegion
serialize comp = do
    putStrLn "\nSERIALIZE"

    clusters  <- Partition.partition comp
    size      <- Size.clusterSize clusters
    ccount    <- Size.componentCount clusters
    buffer    <- Buffer.alloc ccount size

    let dataRegion    = Buffer.dataRegion buffer
    let dataRegionPtr = unwrap dataRegion
    dynDataRegion <- Buffer.dynDataRegion buffer

    putStrLn "\n=== clusters ===" >> pprint clusters

    putStrLn $ "\nsize = " <> show size
    putStrLn $ "\nccount = " <> show ccount

    putStrLn "\n=== encodeStaticRegion ==="
    swizzleMap <- Memory.withUnmanagedPtr dataRegionPtr $ Buffer.encodeStaticRegion clusters

    -- Buffer.encodeStaticRegion clusters

    putStrLn "\n=== swizzleMap ==="
    pprint swizzleMap

    putStrLn "\n=== copyInitializeComps ==="
    Buffer.copyInitializeCompsX @(Graph.ComponentsM m) ccount dataRegion dynDataRegion
    -- memRegion <- Alloc.alloc @comps clusters
    pure ()
    -- serInfo   <- Serialize.serializeClusters @comps clusters memRegion
    -- pure $! serInfo ^. Serialize.memoryRegion
{-# INLINE serialize #-}


