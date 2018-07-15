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
import qualified Data.Mutable.Class2              as Mutable
import qualified Data.Mutable.Plain               as Data
import qualified Data.Property                    as Property
import qualified Data.Storable2                   as Struct
-- import qualified Data.Storable.Definition         as Struct
import qualified Foreign.Marshal.Alloc  as Mem
import qualified Foreign.Marshal.Utils  as Mem
import qualified Foreign.Storable       as StdStorable
import qualified Foreign.Storable.Class as Storable
import qualified Memory                 as Memory
import qualified Type.Known             as Type

import Data.Storable2         (type (-::), Label)
import Data.Storable2         (GenericStruct)
import Foreign.Ptr            (Ptr, minusPtr, nullPtr, plusPtr)
import Foreign.Storable.Class (Copy, Storable, View)
import Foreign.Storable.Utils (Dynamic, Dynamics)
import Foreign.Storable.Utils (castPeekAndOffset, castPokeAndOffset)



----------------------
-- === MemChunk === --
----------------------

-- === Definition === --

-- TODO: remove, replace with Memory.ConstantRegion
newtype MemChunk t (n :: Nat) (a :: Type) = MemChunk (Memory.Ptr t a)
makeLenses ''MemChunk


-- === API === --

unsafeNullMemChunk :: Memory.PtrType t => MemChunk t n a
unsafeNullMemChunk = wrap Memory.nullPtr
{-# INLINE unsafeNullMemChunk #-}


-- === Instances === --

instance (Storable.KnownConstantSize a, Type.KnownInt n)
      => Storable.KnownConstantSize (MemChunk t n a) where
    constantSize = Type.val' @n * Storable.constantSize @a
    {-# INLINE constantSize #-}

-- instance Applicative m
--       => Storable.Peek Struct.Field m (MemChunk t n a) where
--     peek = pure . wrap . coerce
--     {-# INLINE peek #-}



-------------------------
-- === SmallVector === --
-------------------------

-- === Definition === --

newtype SmallVectorA t (alloc :: Memory.Allocator) (n :: Nat) (a :: Type)
      = SmallVector (SmallVector__ t n a)

type SmallVector__ t (n :: Nat) a
   = GenericStruct t (SmallVectorStruct t n a)

data SmallVectorStruct t n a = SmallVectorStruct
    { _fieldSize        :: Int
    , _fieldCapacity    :: Int
    , _fieldExternalMem :: Memory.UnmanagedPtr a
    , _fieldLocalMem    :: MemChunk t n a
    } deriving (Generic)

makeLenses ''SmallVectorA


type instance Item (SmallVectorA t alloc n a) = a
instance Struct.IsStruct (SmallVectorA t alloc n a)
type instance Memory.Management (SmallVectorA t alloc n a) = t


deriving instance Eq     (SmallVector__ t n a) => Eq     (SmallVectorA t alloc n a)
deriving instance Ord    (SmallVector__ t n a) => Ord    (SmallVectorA t alloc n a)
deriving instance NFData (SmallVector__ t n a) => NFData (SmallVectorA t alloc n a)

-- === Aliases === --

type SmallVector t         = SmallVectorA t Memory.StdAllocator
type ManagedSmallVectorA   = SmallVectorA 'Memory.Managed
type ManagedSmallVector    = SmallVector  'Memory.Managed
type UnmanagedSmallVector  = SmallVector  'Memory.Unmanaged
type UnmanagedSmallVectorA = SmallVectorA 'Memory.Unmanaged


-- === Fields === --

fieldSize        :: Struct.Lens (Label "_fieldSize")
fieldCapacity    :: Struct.Lens (Label "_fieldCapacity")
fieldExternalMem :: Struct.Lens (Label "_fieldExternalMem")
fieldLocalMem    :: Struct.Lens (Label "_fieldLocalMem")
fieldSize        = Struct.autoLens ; {-# INLINE fieldSize      #-}
fieldCapacity    = Struct.autoLens ; {-# INLINE fieldCapacity    #-}
fieldExternalMem = Struct.autoLens ; {-# INLINE fieldExternalMem #-}
fieldLocalMem    = Struct.autoLens ; {-# INLINE fieldLocalMem    #-}


-- === Utils === --

localMem :: Memory.PtrType t => SmallVectorA t alloc n a -> MemChunk t n a
localMem = wrap . Memory.coercePtr . unwrap . Struct.ref fieldLocalMem
{-# INLINE localMem #-}

elemsPtr :: (MonadIO m, Memory.PtrType t)
    => SmallVectorA t alloc n a -> m (Memory.UnmanagedPtr a)
elemsPtr = \a -> do
    isExt <- usesDynamicMemory a
    if isExt then Struct.read fieldExternalMem a
             else pure . Memory.unsafeToUnmanaged . unwrap $ localMem a
{-# INLINE elemsPtr #-}

usesDynamicMemory :: (MonadIO m, Memory.PtrType t)
    => SmallVectorA t alloc n a -> m Bool
usesDynamicMemory = fmap (/= Memory.nullPtr) . Struct.read fieldExternalMem
{-# INLINE usesDynamicMemory #-}


-- === API Instances === --

instance Storable.KnownConstantSize (SmallVector__ t n a)
      => Storable.KnownConstantSize (SmallVectorA t alloc n a) where
    constantSize = Storable.constantSize @(SmallVector__ t n a)
    {-# INLINE constantSize #-}

instance (MonadIO m, Memory.PtrType t, Storable.KnownConstantSize a)
      => Storable.KnownSize Storable.Dynamic m (SmallVectorA t alloc n a) where
    size = \a -> usesDynamicMemory a >>= \case
        True  -> (Storable.constantSize @a *) <$> size a
        False -> pure 0
    {-# INLINE size #-}

instance (MonadIO m, Memory.PtrType t, Type.KnownInt n)
      => PlacementNew m (SmallVectorA t alloc n a) where
    placementNew = \ptr -> liftIO $ do
        let a = Struct.unsafeCastFromPtr ptr
        Struct.write fieldSize a 0
        Struct.write fieldCapacity a $! Type.val' @n
        Struct.write fieldExternalMem a Memory.nullPtr
        pure a
    {-# INLINE placementNew #-}

instance
    ( MonadIO m
    , Memory.PtrType t
    , Storable.KnownConstantSize (SmallVectorA t alloc n a)
    , PlacementNew m (SmallVectorA t alloc n a)
    ) => New m (SmallVectorA t alloc n a) where
    new = placementNew =<< Memory.mallocBytes size where
        size = Storable.constantSize @(SmallVectorA t alloc n a)
    {-# INLINE new #-}

instance (MonadIO m, Memory.PtrType t)
      => Size m (SmallVectorA t alloc n a) where
    size = Struct.read fieldSize
    {-# INLINE size #-}

instance (MonadIO m, Memory.PtrType t)
      => Capacity m (SmallVectorA t alloc n a) where
    capacity = Struct.read fieldCapacity
    {-# INLINE capacity #-}

instance (MonadIO m, t ~ 'Memory.Unmanaged)
      => Free m (SmallVectorA t alloc n a) where
    free = \a -> do
        whenM (usesDynamicMemory a)
            $ Memory.free =<< Struct.read fieldExternalMem a
        Struct.free a
    {-# INLINE free #-}

instance (MonadIO m, Memory.PtrType t, Storable.StaticPeek View m a)
      => Read m (SmallVectorA t alloc n a) where
    unsafeRead = \a ix -> do
        ptr <- elemsPtr a
        Storable.peekElemOff @View ptr ix
    {-# INLINE unsafeRead #-}

instance (MonadIO m, Memory.PtrType t, Storable.StaticPoke View m a)
      => Write m (SmallVectorA t alloc n a) where
    unsafeWrite = \a ix val -> do
        ptr <- elemsPtr a
        Storable.pokeElemOff @View ptr ix val
    {-# INLINE unsafeWrite #-}

instance (MonadIO m, Memory.PtrType t, Storable.StaticPeek View m a)
      => ToList m (SmallVectorA t alloc n a) where
    toList = \a -> do
        len <- size a
        traverse (unsafeRead a) [0 .. len - 1]
    {-# INLINE toList #-}

instance (Monad m, New m (SmallVectorA t alloc n a), PushBack m (SmallVectorA t alloc n a))
      => FromList m (SmallVectorA t alloc n a) where
    fromList = \lst -> do
        a <- new
        traverse_ (pushBack a) lst
        pure a
    {-# INLINE fromList #-}

instance (MonadIO m, Memory.PtrType t, Storable.KnownConstantSize a, Memory.UnmanagedAllocation alloc a m)
      => Grow m (SmallVectorA t alloc n a) where
    grow = \a -> do
        oldCapacity <- capacity a
        elemCount   <- size     a
        ptr         <- elemsPtr a
        let newSize       = if oldCapacity == 0 then 16 else oldCapacity * 2
            elemByteSize  = Storable.constantSize @a
            bytesToCopy   = elemByteSize * elemCount
        (newElemsPtr_ :: Memory.UnmanagedPtr a) <- Memory.allocate @alloc newSize
        let newElemsPtr = coerce (unwrap newElemsPtr_)
        Memory.copyBytes newElemsPtr ptr bytesToCopy
        whenM (usesDynamicMemory a) $ Memory.free ptr
        Struct.write fieldCapacity a newSize
        Struct.write fieldExternalMem a $! newElemsPtr
    {-# INLINE grow #-}

instance (MonadIO m, Memory.PtrType t, Write m (SmallVectorA t alloc n a), Grow m (SmallVectorA t alloc n a))
      => PushBack m (SmallVectorA t alloc n a) where
    pushBack = \a v -> do
        siz <- size     a
        cap <- capacity a
        when (siz == cap) $ grow a
        unsafeWrite a siz v
        Struct.write fieldSize a $! siz + 1
    {-# INLINE pushBack #-}

instance
    ( MonadIO m
    , Memory.PtrType t
    , Grow  m (SmallVectorA t alloc n a)
    , Write m (SmallVectorA t alloc n a)
    , Storable.KnownConstantSize a
    ) => InsertAt m (SmallVectorA t alloc n a) where
    insertAt = \a ix v -> do
        siz <- size     a
        cap <- capacity a
        when (siz == cap) $ grow a
        ptr0 <- elemsPtr a
        let elSize  = Storable.constantSize @a
            ptrIx   = ptr0  `Memory.plus` (elSize * ix)
            ptrIx'  = ptrIx `Memory.plus` elSize
            byteOff = elSize * (siz - ix)
        when (byteOff > 0) $ Memory.moveBytes ptrIx' ptrIx byteOff
        unsafeWrite a ix v
        Struct.write fieldSize a $! siz + 1
    {-# INLINE insertAt #-}

instance
    ( MonadIO m
    , Memory.PtrType t
    , Storable.KnownConstantSize a
    ) => RemoveAt m (SmallVectorA t alloc n a) where
    removeAt = \a ix -> do
        siz  <- size a
        ptr0 <- elemsPtr a
        let elSize  = Storable.constantSize @a
            ptrIx   = ptr0  `Memory.plus` (elSize * ix)
            ptrIx'  = ptrIx `Memory.plus` elSize
            byteOff = elSize * (siz - ix - 1)
        when (byteOff > 0) $ liftIO $ Memory.moveBytes ptrIx ptrIx' byteOff
        Struct.write fieldSize a $! siz - 1
    {-# INLINE removeAt #-}

instance (MonadIO m, Memory.PtrType t, IxMap m (SmallVectorA t alloc n a))
      => Map m (SmallVectorA t alloc n a) where
    mapM = \a f -> do
        siz <- size a
        let go i = if i == siz then pure () else unsafeMapAtM_ a i f >> go (i + 1)
        go 0
    {-# INLINE mapM #-}


newtype Field  a = Field  (Memory.UnmanagedPtr a)
newtype Field1 a = Field1 (∀ t1. Memory.UnmanagedPtr (a t1))

instance (Storable.KnownConstantSize a, Mutable.Unswizzle m (Field a), MonadIO m, Memory.PtrType t)
      => Mutable.Unswizzle m (SmallVectorA t alloc n a) where
    unswizzle = \a -> do
        ptr0 <- elemsPtr a
        siz  <- size a
        let elemSize = Storable.constantSize @a
            go i ptr = if i == siz then pure () else do
                Mutable.unswizzle (Field @a ptr)
                go (i + 1) $! ptr `Memory.plus` elemSize
        go 0 ptr0
    {-# INLINE unswizzle #-}


-- === Memory management instances === --

instance Applicative m
      => Storable.Peek View m (SmallVectorA 'Memory.Unmanaged alloc n a) where
    peek = pure . coerce
    {-# INLINE peek #-}

instance (t ~ 'Memory.Unmanaged, MonadIO m, Storable.KnownConstantSize (SmallVectorA t alloc n a), Show (SmallVectorA t alloc n a))
      => Storable.Poke View m (SmallVectorA 'Memory.Unmanaged alloc n a) where
    poke = \ptr a ->
        let size = Storable.constantSize @(SmallVectorA t alloc n a)
        in  Memory.copyBytes ptr (coerce a) size
    {-# INLINE poke #-}


instance
    ( MonadIO m
    , Memory.PtrType t
    , Storable.KnownConstantSize a
    , Memory.UnmanagedAllocation alloc a m
    ) => Data.CopyInitializer m (SmallVectorA t alloc n a) where
    copyInitialize = \a -> whenM_ (usesDynamicMemory a) $ do
        cap       <- capacity a
        elemCount <- size     a
        ptr       <- elemsPtr a
        let elemByteSize  = Storable.constantSize @a
            bytesToCopy   = elemByteSize * elemCount
        (newElemsPtr :: Memory.UnmanagedPtr a) <- Memory.allocate @alloc elemCount
        let newElemsPtr' = coerce (unwrap newElemsPtr)
        Memory.copyBytes newElemsPtr' ptr bytesToCopy
        Struct.write fieldExternalMem a newElemsPtr'
        Struct.write fieldCapacity    a elemCount
    {-# INLINE copyInitialize #-}


-- === Debug instances === --

deriving instance Show (SmallVector__ t n a) => Show (SmallVectorA t alloc n a)


-- === Deprecated instances === --

type instance Property.Get Dynamics (SmallVectorA t alloc n a) = Dynamic

instance
    ( Storable.Peek View IO a
    , Storable.KnownConstantSize (SmallVectorA 'Memory.Unmanaged alloc n a)
    , Storable View IO (SmallVectorA 'Memory.Unmanaged alloc n a)
    , Type.KnownInt n
    ) => StdStorable.Storable (SmallVectorA 'Memory.Unmanaged alloc n a) where
    sizeOf    = \ ~_ -> Storable.constantSize @(SmallVectorA 'Memory.Unmanaged alloc n a)
    alignment = \ ~_ -> StdStorable.alignment (undefined :: Int)
    peek      = Storable.peek @View . wrap
    poke      = Storable.poke @View . wrap
    {-# INLINE sizeOf #-}


-- WARNING: this instance is strange. It does not release self-memory,
--          because it is used for placement-new objects
instance (MonadIO m, Memory.PtrType t)
      => Data.Destructor1 m (SmallVectorA t alloc n) where
    destruct1 = \a -> liftIO $ do
        whenM (usesDynamicMemory a)
            $ Memory.free =<< Struct.read fieldExternalMem a
    {-# INLINE destruct1 #-}

-- WARNING: this instance is strange. It does not release self-memory,
--          because it is used for placement-new objects
instance MonadIO m
      => Data.ShallowDestructor1 m (SmallVectorA 'Memory.Unmanaged alloc n) where
    destructShallow1 = const $ pure () -- free
    {-# INLINE destructShallow1 #-}
