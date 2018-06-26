{-# LANGUAGE UndecidableInstances #-}

module Data.Mutable.Storable.SmallAutoVector
    (module Data.Mutable.Storable.SmallAutoVector, module X) where
import Data.Mutable.Class as X

import Prologue hiding (FromList, Read, ToList, empty, length, toList,
                 unsafeRead, w)

import qualified Data.AutoVector.Mutable.Storable as Vector
import qualified Data.Construction                as Data
import qualified Data.Convert2.Class              as Convert
import qualified Data.List                        as List
import qualified Data.Mutable.Plain               as Data
import qualified Data.Property                    as Property
import qualified Data.Storable                    as Struct
import qualified Foreign.Marshal.Alloc            as Mem
import qualified Foreign.Marshal.Utils            as Mem
import qualified Foreign.Storable                 as StdStorable
import qualified Foreign.Storable.Class           as Storable
import qualified Memory                           as Memory
import qualified Type.Known                       as Type

import Data.Storable          (type (-::), UnmanagedStruct)
import Foreign.Ptr            (Ptr, minusPtr, nullPtr, plusPtr)
import Foreign.Storable.Class (Copy, Storable, View)
import Foreign.Storable.Utils (Dynamic, Dynamics)
import Foreign.Storable.Utils (castPeekAndOffset, castPokeAndOffset)
import System.IO.Unsafe       (unsafeDupablePerformIO, unsafePerformIO)



----------------------
-- === MemChunk === --
----------------------

-- === Definition === --

-- TODO: remove, replace with Memory.ConstantRegion
newtype MemChunk (n :: Nat) (a :: Type) = MemChunk (Ptr a)
makeLenses ''MemChunk


-- === API === --

unsafeNullMemChunk :: MemChunk n a
unsafeNullMemChunk = wrap nullPtr
{-# INLINE unsafeNullMemChunk #-}


-- === Instances === --

instance (Storable.KnownConstantSize a, Type.KnownInt n)
      => Storable.KnownConstantSize (MemChunk n a) where
    constantSize = Type.val' @n * Storable.constantSize @a
    {-# INLINE constantSize #-}

instance Applicative m
      => Storable.Peek Struct.Field m (MemChunk n a) where
    peek = pure . coerce
    {-# INLINE peek #-}



-------------------------
-- === SmallVector === --
-------------------------

-- === Definition === --

type    SmallVector = SmallVectorA Memory.StdAllocator
newtype SmallVectorA (alloc :: Memory.Allocator) (n :: Nat) (a :: Type)
      = SmallVector (SmallVector__ n a)
    deriving (Eq, Ord, NFData)

type SmallVector__ (n :: Nat) a = UnmanagedStruct (Layout n a)
type Layout n a =
   '[ "length"      -:: Int
    , "capacity"    -:: Int
    , "externalMem" -:: (Ptr a)
    , "localMem"    -:: MemChunk n a
    ]
makeLenses ''SmallVectorA

type instance Item (SmallVectorA alloc n a) = a
instance Struct.IsStruct (SmallVectorA alloc n a)
type instance Memory.Management (SmallVectorA alloc n a) = 'Memory.Unmanaged


-- === Fields === --

_length      :: Struct.FieldRef "length"
_capacity    :: Struct.FieldRef "capacity"
_externalMem :: Struct.FieldRef "externalMem"
_localMem    :: Struct.FieldRef "localMem"
_length      = Struct.field ; {-# INLINE _length      #-}
_capacity    = Struct.field ; {-# INLINE _capacity    #-}
_externalMem = Struct.field ; {-# INLINE _externalMem #-}
_localMem    = Struct.field ; {-# INLINE _localMem    #-}


-- === Utils === --

elemsPtr :: MonadIO m => SmallVectorA alloc n a -> m (Ptr a)
elemsPtr = \a -> do
    isExt <- usesDynamicMemory a
    if isExt then            Struct.readField _externalMem a
             else coerce <$> Struct.readField _localMem    a
{-# INLINE elemsPtr #-}

usesDynamicMemory :: MonadIO m => SmallVectorA alloc n a -> m Bool
usesDynamicMemory = fmap (/= nullPtr) . Struct.readField _externalMem
{-# INLINE usesDynamicMemory #-}


-- === API Instances === --

instance Storable.KnownConstantSize (SmallVector__ n a)
      => Storable.KnownConstantSize (SmallVectorA alloc n a) where
    constantSize = Storable.constantSize @(SmallVector__ n a)
    {-# INLINE constantSize #-}

instance (MonadIO m, Storable.KnownConstantSize a)
      => Storable.KnownSize Storable.Dynamic m (SmallVectorA alloc n a) where
    size = \a -> usesDynamicMemory a >>= \case
        True  -> (Storable.constantSize @a *) <$> size a
        False -> pure 0
    {-# INLINE size #-}

instance (MonadIO m, Type.KnownInt n)
      => PlacementNew m (SmallVectorA alloc n a) where
    placementNew = \ptr -> liftIO $ do
        let a = Struct.unsafeCastFromPtr (Convert.convert (coerce ptr :: Ptr ()))
        Struct.writeField _length a 0
        Struct.writeField _capacity a $! Type.val' @n
        Struct.writeField _externalMem a nullPtr
        pure a
    {-# INLINE placementNew #-}

instance
    ( MonadIO m
    , Storable.KnownConstantSize (SmallVectorA alloc n a)
    , Type.KnownInt n
    ) => New m (SmallVectorA alloc n a) where
    new = do
        ptr <- liftIO . Mem.mallocBytes
             $ Storable.constantSize @(SmallVectorA alloc n a)
        placementNew ptr
    {-# INLINE new #-}

instance MonadIO m
      => Size m (SmallVectorA alloc n a) where
    size = Struct.readField _length
    {-# INLINE size #-}

instance MonadIO m
      => Capacity m (SmallVectorA alloc n a) where
    capacity = Struct.readField _capacity
    {-# INLINE capacity #-}

instance MonadIO m
      => Free m (SmallVectorA alloc n a) where
    free = \a -> liftIO $ do
        whenM (usesDynamicMemory a)
            $ Mem.free =<< Struct.readField _externalMem a
        Struct.free a
    {-# INLINE free #-}

instance (MonadIO m, Storable.StaticPeek View m a)
      => Read m (SmallVectorA alloc n a) where
    unsafeRead = \a ix -> do
        ptr <- elemsPtr a
        Storable.peekElemOff @View ptr ix
    {-# INLINE unsafeRead #-}

instance (MonadIO m, Storable.StaticPoke View m a)
      => Write m (SmallVectorA alloc n a) where
    unsafeWrite = \a ix val -> do
        ptr <- elemsPtr a
        Storable.pokeElemOff @View ptr ix val
    {-# INLINE unsafeWrite #-}

instance (MonadIO m, Storable.StaticPeek View m a)
      => ToList m (SmallVectorA alloc n a) where
    toList = \a -> do
        len <- size a
        mapM (unsafeRead a) [0 .. len - 1]
    {-# INLINE toList #-}

instance (Monad m, New m (SmallVectorA alloc n a), PushBack m (SmallVectorA alloc n a))
      => FromList m (SmallVectorA alloc n a) where
    fromList = \lst -> do
        a <- new
        mapM_ (pushBack a) lst
        pure a
    {-# INLINE fromList #-}

instance (MonadIO m, Storable.KnownConstantSize a, Memory.Allocation alloc a m)
      => Grow m (SmallVectorA alloc n a) where
    grow = \a -> do
        oldCapacity <- capacity a
        elemCount   <- size     a
        ptr         <- elemsPtr a
        let newSize       = if oldCapacity == 0 then 16 else oldCapacity * 2
            elemByteSize  = Storable.constantSize @a
            bytesToCopy   = elemByteSize * elemCount
        (newElemsPtr_ :: Memory.UnmanagedPtr a) <- Memory.allocate @alloc newSize
        let newElemsPtr = coerce (unwrap newElemsPtr_)
        liftIO $ Mem.copyBytes newElemsPtr ptr bytesToCopy
        whenM (usesDynamicMemory a) $
            liftIO (Mem.free ptr)
        Struct.writeField _capacity a newSize
        Struct.writeField _externalMem a $! newElemsPtr
    {-# INLINE grow #-}

instance (MonadIO m, Write m (SmallVectorA alloc n a), Grow m (SmallVectorA alloc n a))
      => PushBack m (SmallVectorA alloc n a) where
    pushBack = \a v -> do
        siz <- size     a
        cap <- capacity a
        when (siz == cap) $ grow a
        unsafeWrite a siz v
        Struct.writeField _length a $! siz + 1
    {-# INLINE pushBack #-}

instance
    ( MonadIO m
    , Grow  m (SmallVectorA alloc n a)
    , Write m (SmallVectorA alloc n a)
    , Storable.KnownConstantSize a
    ) => InsertAt m (SmallVectorA alloc n a) where
    insertAt = \a ix v -> do
        siz <- size     a
        cap <- capacity a
        when (siz == cap) $ grow a
        ptr0 <- elemsPtr a
        let elSize  = Storable.constantSize @a
            ptrIx   = ptr0  `plusPtr` (elSize * ix)
            ptrIx'  = ptrIx `plusPtr` elSize
            byteOff = elSize * (siz - ix)
        when (byteOff > 0) $ liftIO $ Mem.moveBytes ptrIx' ptrIx byteOff
        unsafeWrite a ix v
        Struct.writeField _length a $! siz + 1
    {-# INLINE insertAt #-}

instance
    ( MonadIO m
    , Storable.KnownConstantSize a
    ) => RemoveAt m (SmallVectorA alloc n a) where
    removeAt = \a ix -> do
        siz  <- size a
        ptr0 <- elemsPtr a
        let elSize  = Storable.constantSize @a
            ptrIx   = ptr0  `plusPtr` (elSize * ix)
            ptrIx'  = ptrIx `plusPtr` elSize
            byteOff = elSize * (siz - ix - 1)
        when (byteOff > 0) $ liftIO $ Mem.moveBytes ptrIx ptrIx' byteOff
        Struct.writeField _length a $! siz - 1
    {-# INLINE removeAt #-}



-- === Memory management instances === --

instance Applicative m
      => Storable.Peek View m (SmallVectorA alloc n a) where
    peek = pure . coerce
    {-# INLINE peek #-}

instance (MonadIO m, Storable.KnownConstantSize (SmallVectorA alloc n a), Show (SmallVectorA alloc n a))
      => Storable.Poke View m (SmallVectorA alloc n a) where
    poke = \ptr a ->
        let size = Storable.constantSize @(SmallVectorA alloc n a)
        in  liftIO $ Mem.copyBytes ptr (coerce a) size

    {-# INLINE poke #-}

instance MonadIO m => Data.ShallowDestructor1 m (SmallVectorA alloc n) where
    destructShallow1 = free
    {-# INLINE destructShallow1 #-}

instance
    ( MonadIO m
    , Storable.KnownConstantSize a
    , Memory.Allocation alloc a m
    ) => Data.CopyInitializer m (SmallVectorA alloc n a) where
    copyInitialize = \a -> whenM_ (usesDynamicMemory a) $ do
        cap       <- capacity a
        elemCount <- size     a
        ptr       <- elemsPtr a
        let elemByteSize  = Storable.constantSize @a
            bytesToCopy   = elemByteSize * elemCount
        (newElemsPtr :: Memory.UnmanagedPtr a) <- Memory.allocate @alloc cap
        let newElemsPtr' = coerce (unwrap newElemsPtr)
        liftIO $ Mem.copyBytes newElemsPtr' ptr bytesToCopy
        Struct.writeField _externalMem a $! newElemsPtr'
    {-# INLINE copyInitialize #-}


-- === Debug instances === --

instance (Show a, ToList IO (SmallVectorA alloc n a))
      => Show (SmallVectorA alloc n a) where
    show = show . unsafePerformIO . toList



-- === Deprecated instances === --

type instance Property.Get Dynamics (SmallVectorA alloc n a) = Dynamic

instance
    ( Storable.Peek View IO a
    , Storable.KnownConstantSize (SmallVectorA alloc n a)
    , Storable View IO (SmallVectorA alloc n a)
    , Type.KnownInt n
    ) => StdStorable.Storable (SmallVectorA alloc n a) where
    sizeOf    = \ ~_ -> Storable.constantSize @(SmallVectorA alloc n a)
    alignment = \ ~_ -> StdStorable.alignment (undefined :: Int)
    peek      = Storable.peek @View
    poke      = Storable.poke @View
    {-# INLINE sizeOf #-}


-- WARNING: this instance is strange. It does not release self-memory,
--          because it is used for placement-new objects
instance MonadIO m
      => Data.Destructor1 m (SmallVectorA alloc n) where
    destruct1 = \a -> liftIO $ do
        whenM (usesDynamicMemory a)
            $ Mem.free =<< Struct.readField _externalMem a
    {-# INLINE destruct1 #-}

