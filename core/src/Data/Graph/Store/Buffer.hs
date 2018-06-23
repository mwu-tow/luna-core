{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Store.Buffer where

import Prologue hiding (Data)

import qualified Data.ByteString.Internal    as ByteString
import qualified Data.Convert2               as Convert
import qualified Data.Graph.Data.Graph.Class as Graph
import qualified Data.Graph.Store.Size.Class as Size
import qualified Data.Storable               as Struct
import qualified Foreign.ForeignPtr          as ForeignPtr
import qualified Foreign.Storable.Class      as Storable
import qualified Foreign.Storable.Deriving   as Storable
import qualified Foreign.Storable.Utils      as Storable
import qualified Memory                      as Memory
import qualified Type.Data.List              as List

import Data.ByteString             (ByteString)
import Data.Graph.Store.Size.Class (Size)
import Data.Storable               (type (-::), ManagedStruct)
import Foreign.ForeignPtr          (touchForeignPtr)
import Foreign.ForeignPtr.Unsafe   (unsafeForeignPtrToPtr)
import Foreign.ForeignPtr.Utils    (SomeForeignPtr)
import Foreign.Ptr.Utils           (SomePtr)
import Type.Data.Semigroup         (type (<>))



------------------------------
-- === DynamicMemRegion === --
------------------------------

-- === Definition === --

newtype UnknownSizeMemRegion t a = UnknownSizeMemRegion (Memory.Ptr t a)

unsafeNull :: Memory.NullPtr t => UnknownSizeMemRegion t a
unsafeNull = UnknownSizeMemRegion Memory.nullPtr
{-# INLINE unsafeNull #-}

-- FIXME: change it to Struct.FieldInitializer
instance Applicative m
      => Storable.Poke Struct.Field m (UnknownSizeMemRegion t a) where
    poke = \_ _ -> pure ()
    {-# INLINE poke #-}



--------------------------
-- === MemoryRegion === --
--------------------------

-- === Definition === --

newtype Buffer graph = Buffer (ManagedStruct (BufferLayout graph))

type HeaderLayout graph =
   '[ "staticDataRegionSize"  -:: Int
    , "dynamicDataRegionSize" -:: Int
    , "pointerDataRegionSize" -:: Int
    , "componentElems"        -:: Memory.ConstantRegion
                                  (List.Length (Graph.Components graph))
                                  'Memory.Managed Int
    ]

type BufferDataRegion = UnknownSizeMemRegion 'Memory.Managed ()

type BufferLayout graph =
    HeaderLayout graph <>
   '[ "memoryRegion" -:: BufferDataRegion
    ]

makeLenses ''Buffer
instance Struct.IsStruct (Buffer graph)
type instance Memory.Management (Buffer graph) = 'Memory.Managed


-- === Fields === --

field_staticDataRegionSize  :: Struct.FieldRef "staticDataRegionSize"
field_dynamicDataRegionSize :: Struct.FieldRef "dynamicDataRegionSize"
field_pointerDataRegionSize :: Struct.FieldRef "pointerDataRegionSize"
field_memoryRegion          :: Struct.FieldRef "memoryRegion"
field_staticDataRegionSize  = Struct.field ; {-# INLINE field_staticDataRegionSize  #-}
field_dynamicDataRegionSize = Struct.field ; {-# INLINE field_dynamicDataRegionSize #-}
field_pointerDataRegionSize = Struct.field ; {-# INLINE field_pointerDataRegionSize #-}
field_memoryRegion          = Struct.field ; {-# INLINE field_memoryRegion          #-}


type HasDataRegion graph = Struct.HasField "memoryRegion" (Buffer graph)
dataRegion :: HasDataRegion graph => Buffer graph -> BufferDataRegion
dataRegion = coerce . Struct.fieldPtr field_memoryRegion
{-# INLINE dataRegion #-}


-- === API === --

type Alloc m =
    ( MonadIO m
    , Storable.KnownConstantStaticSize (HeaderLayout (Graph.Discover m))
    )
alloc :: ∀ m. Alloc m => Size -> m (Buffer (Graph.Discover m))
alloc = \size -> liftIO $ do
    let headerSize = Storable.constantStaticSize @(HeaderLayout (Graph.Discover m))
        bodySize   = Size.total size
        totalSize  = headerSize + bodySize
    ptr <- ForeignPtr.mallocForeignPtrBytes totalSize
    undefined
    -- Struct.placementNew @(Buffer (Graph.Discover m))
    --     (Convert.convert ptr)
    --     (size ^. Size.static)
    --     (size ^. (Size.dynamic . Size.dataRegion))
    --     (size ^. (Size.dynamic . Size.ptrRegion))
    --     unsafeNull


-- === Conversions === --

unsafeFreeze :: (MonadIO m, HasDataRegion graph) => Buffer graph -> m ByteString
unsafeFreeze = \a -> do
    staticSize  <- Struct.readField field_staticDataRegionSize  a
    dynDataSize <- Struct.readField field_dynamicDataRegionSize a
    dynPtrSize  <- Struct.readField field_pointerDataRegionSize a
    let mem = dataRegion a
    let totalSize = staticSize + dynDataSize + dynPtrSize
    pure $ ByteString.PS (coerce mem) 0 totalSize

unsafeThaw :: Monad m => ByteString -> m (Buffer (Graph.Discover m))
unsafeThaw = \(ByteString.PS ptr _ _) -> pure $ coerce ptr
{-# INLINE unsafeThaw #-}


-- newtype Header = Header
--     { _size :: Size
--     } deriving (Show)
-- makeLenses      ''Header
-- Storable.derive ''Header

-- data Buffer = Buffer
--     { _header :: Header
--     , _block  :: SomeForeignPtr
--     } deriving (Show)
-- makeLenses ''Buffer

-- data BufferView = BufferView
--     { _static  :: SomePtr
--     , _dynData :: SomePtr
--     , _dynPtrs :: SomePtr
--     } deriving (Show)
-- makeLenses ''BufferView


-- alloc :: MonadIO m => Size -> m Buffer
-- alloc = \size -> do
--     let headerSize = Storable.sizeOf' @Header
--         bodySize   = Size.total size
--         totalSize  = headerSize + bodySize
--     ptr <- liftIO $ ForeignPtr.mallocForeignPtrBytes totalSize
--     let bodyPtr = ForeignPtr.plusForeignPtr ptr headerSize
--     pure $ Buffer (Header size) bodyPtr
-- {-# INLINE alloc #-}


-- === API === --

-- instance Convert.To ByteString Buffer where
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
