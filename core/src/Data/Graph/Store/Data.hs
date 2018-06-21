{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Store.Data where

import Prologue hiding (Data)

import qualified Data.ByteString.Internal    as ByteString
import qualified Data.Convert2               as Convert
import qualified Data.Graph.Store.Size.Class as Size
import qualified Foreign.ForeignPtr          as ForeignPtr
import qualified Foreign.Storable.Deriving   as Storable
import qualified Foreign.Storable.Utils      as Storable

import Data.ByteString             (ByteString)
import Data.Graph.Store.Size.Class (Size)
import Foreign.ForeignPtr          (touchForeignPtr)
import Foreign.ForeignPtr.Unsafe   (unsafeForeignPtrToPtr)
import Foreign.ForeignPtr.Utils    (SomeForeignPtr)
import Foreign.Ptr.Utils           (SomePtr)


--------------------------
-- === MemoryRegion === --
--------------------------

-- === Definition === --


newtype Header = Header
    { _size :: Size
    } deriving (Show)
makeLenses      ''Header
Storable.derive ''Header

data Binary = Binary
    { _header :: Header
    , _block  :: SomeForeignPtr
    } deriving (Show)
makeLenses ''Binary

data BinaryView = BinaryView
    { _static  :: SomePtr
    , _dynData :: SomePtr
    , _dynPtrs :: SomePtr
    } deriving (Show)
makeLenses ''BinaryView


alloc :: MonadIO m => Size -> m Binary
alloc = \size -> do
    let headerSize = Storable.sizeOf' @Header
        bodySize   = Size.total size
        totalSize  = headerSize + bodySize
    ptr <- liftIO $ ForeignPtr.mallocForeignPtrBytes totalSize
    let bodyPtr = ForeignPtr.plusForeignPtr ptr headerSize
    pure $ Binary (Header size) bodyPtr
{-# INLINE alloc #-}


-- === API === --

-- instance Convert.To ByteString Binary where
--     to = \(Data ptr size) -> ByteString.PS (coerce ptr) 0 (Size.total size)
--     {-# INLINE to #-}

-- data Dynamic = Dynamic
--     { _noPointersMem :: SomePtr
--     , _pointersMem   :: SomePtr
--     } deriving (Show)
-- makeLenses ''Dynamic

-- type DynamicMemVariant = Lens' Dynamic SomePtr

-- unsafeMakeRaw :: Data -> Raw
-- unsafeMakeRaw = \(Data staticMem dynamicMem ptrsMem) ->
--     let staticPtr  = unsafeForeignPtrToPtr staticMem
--         dynamicPtr = unsafeForeignPtrToPtr dynamicMem
--         ptrsPtr    = unsafeForeignPtrToPtr ptrsMem
--     in Raw staticPtr dynamicPtr ptrsPtr
-- {-# INLINE unsafeMakeRaw #-}

-- touch :: MonadIO m => Data -> m ()
-- touch = \(Data staticMem dynamicMem ptrsMem) -> liftIO $! do
--     touchForeignPtr staticMem
--     touchForeignPtr dynamicMem
--     touchForeignPtr ptrsMem
-- {-# INLINE touch #-}

-- withRaw :: MonadIO m
--         => Data -> (Raw -> m Raw)
--         -> m Data
-- withRaw = \memReg f -> do
--     let rawMemReg = unsafeMakeRaw memReg
--     _ <- f rawMemReg
--     touch memReg
--     pure $! memReg
-- {-# INLINE withRaw #-}

-- viewDynamic :: Raw -> Dynamic
-- viewDynamic = \(Raw _ dm dpm) -> Dynamic dm dpm
-- {-# INLINE viewDynamic #-}

-- constructRaw :: Dynamic -> SomePtr -> Raw
-- constructRaw = \(Dynamic dm dpm) ptr -> Raw ptr dm dpm
-- {-# INLINE constructRaw #-}


-- data Data = Data
--     { _staticMem  :: SomeForeignPtr
--     , _dynamicMem     :: SomeForeignPtr
--     , _dynamicPtrsMem :: SomeForeignPtr
--     } deriving (Show)
-- makeLenses ''Data
