{-# LANGUAGE UndecidableInstances #-}

module Data.SmallAutoVector.Mutable.Storable
    (module Data.SmallAutoVector.Mutable.Storable, module X) where
import Data.Mutable.Class as X

import Prologue hiding (FromList, Read, ToList, empty, length, toList,
                 unsafeRead, w)

import qualified Data.AutoVector.Mutable.Storable as Vector
import qualified Data.Construction                as Data
import qualified Data.List                        as List
import qualified Data.Property                    as Property
import qualified Data.Storable                    as Struct
import qualified Foreign.Marshal.Alloc            as Mem
import qualified Foreign.Marshal.Utils            as Mem
import qualified Foreign.Storable                 as StdStorable
import qualified Foreign.Storable.Class           as Storable
import qualified Type.Known                       as Type

import Data.Storable          (type (-::), Struct)
import Foreign.Ptr            (Ptr, minusPtr, nullPtr, plusPtr)
import Foreign.Storable.Class (Copy, Storable, View)
import Foreign.Storable.Utils (Dynamic, Dynamics)
import Foreign.Storable.Utils (castPeekAndOffset, castPokeAndOffset)
import System.IO.Unsafe       (unsafeDupablePerformIO, unsafePerformIO)



----------------------
-- === MemChunk === --
----------------------

-- === Definition === --

newtype MemChunk (n :: Nat) (a :: Type) = MemChunk (Ptr a)
makeLenses ''MemChunk


-- === API === --

unsafeNullMemChunk :: MemChunk n a
unsafeNullMemChunk = wrap nullPtr
{-# INLINE unsafeNullMemChunk #-}


-- === Instances === --

type instance Storable.ConstantSize Storable.Static (MemChunk n a)
            = Storable.ConstantSize Storable.Static a * n

-- instance (Storable.KnownStaticSize a, Type.KnownInt n)
--       => Storable.KnownSize Storable.Static (MemChunk n a) where
--     size = Type.val' @n * Storable.staticSize @a
--     {-# INLINE size #-}

instance Applicative m
      => Storable.Peek t m (MemChunk n a) where
    peek = pure . coerce
    {-# INLINE peek #-}



-------------------------
-- === SmallVector === --
-------------------------

-- === Definition === --

type    SmallVector__ (n :: Nat) a = Struct (Layout n a)
newtype SmallVector   (n :: Nat) a = SmallVector (SmallVector__ n a)
    deriving (Eq, Ord, NFData)

type Layout n a =
   '[ "length"   -:: Int
    , "capacity" -:: Int
    , "offset"   -:: Int
    , "localMem" -:: MemChunk n a
    ]
makeLenses ''SmallVector

type instance Item (SmallVector n a) = a
instance Struct.IsStruct (SmallVector n a)


-- === Fields === --

_length   :: Struct.FieldRef "length"
_capacity :: Struct.FieldRef "capacity"
_offset   :: Struct.FieldRef "offset"
_localMem :: Struct.FieldRef "localMem"
_length   = Struct.field ; {-# INLINE _length   #-}
_capacity = Struct.field ; {-# INLINE _capacity #-}
_offset   = Struct.field ; {-# INLINE _offset   #-}
_localMem = Struct.field ; {-# INLINE _localMem #-}


-- === Utils === --

elemsPtr :: MonadIO m => SmallVector n a -> m (Ptr a)
elemsPtr = \a -> do
    offset <- Struct.readField _offset a
    let localMemPtr = Struct.fieldPtr _localMem a
    pure $ localMemPtr `plusPtr` offset
{-# INLINE elemsPtr #-}

usesDynamicMemory :: MonadIO m => SmallVector n a -> m Bool
usesDynamicMemory = fmap (/= 0) . Struct.readField _offset
{-# INLINE usesDynamicMemory #-}


-- === API Instances === --

-- instance (Type.KnownInt n, Storable.KnownStaticSize t a)
--       => Storable.KnownStaticSize t (SmallVector n a) where
--     staticSize = Storable.staticSize @t @Int                -- length
--                + Storable.staticSize @t @Int                -- capacity
--                + Storable.staticSize @t @Int                -- ptrOff
--                + (Storable.staticSize @t @a * Type.val' @n) -- elems
--     {-# INLINE staticSize #-}
type instance Storable.ConstantSize t (SmallVector n a)
            = Storable.ConstantSize t (SmallVector__ n a)

instance MonadIO m
      => Storable.KnownSize Storable.Dynamic m (SmallVector n a) where
    size = \a -> usesDynamicMemory a >>= \case
        True  -> size a
        False -> pure 0
    {-# INLINE size #-}

instance (MonadIO m, Type.KnownInt n)
      => PlacementNew m (SmallVector n a) where
    placementNew = \ptr -> liftIO $ do
        let vec = Struct.unsafeCastFromPtr ptr
        Struct.writeField _length vec 0
        Struct.writeField _capacity   vec $! Type.val' @n
        Struct.writeField _offset vec 0
        pure vec
    {-# INLINE placementNew #-}

instance
    ( MonadIO m
    , Storable.KnownConstantStaticSize (SmallVector n a)
    , Type.KnownInt n
    ) => New m (SmallVector n a) where
    new = do
        ptr <- liftIO . Mem.mallocBytes
             $ Storable.constantStaticSize @(SmallVector n a)
        placementNew ptr
    {-# INLINE new #-}

instance MonadIO m
      => Size m (SmallVector n a) where
    size = Struct.readField _length
    {-# INLINE size #-}

instance MonadIO m
      => Capacity m (SmallVector n a) where
    capacity = Struct.readField _capacity
    {-# INLINE capacity #-}

instance MonadIO m
      => Free m (SmallVector n a) where
    free = \a -> liftIO $ do
        whenM (usesDynamicMemory a) $ do
            let localMemPtr = Struct.fieldPtr _localMem a
            offset <- Struct.readField _offset a
            Mem.free $ localMemPtr `plusPtr` offset
        Struct.free a
    {-# INLINE free #-}

instance (MonadIO m, Storable.StaticPeek View m a)
      => Read m (SmallVector n a) where
    unsafeRead = \a ix -> do
        ptr <- elemsPtr a
        Storable.peekElemOff @View ptr ix
    {-# INLINE unsafeRead #-}

instance (MonadIO m, Storable.StaticPoke View m a)
      => Write m (SmallVector n a) where
    unsafeWrite = \a ix val -> do
        ptr <- elemsPtr a
        Storable.pokeElemOff @View ptr ix val
    {-# INLINE unsafeWrite #-}

instance (MonadIO m, Storable.StaticPeek View m a)
      => ToList m (SmallVector n a) where
    toList = \a -> do
        len <- size a
        mapM (unsafeRead a) [0 .. len - 1]
    {-# INLINE toList #-}

instance (New m (SmallVector n a), Write m (SmallVector n a), Monad m)
      => FromList m (SmallVector n a) where
    fromList = \lst -> do
        a <- new
        mapM_ (uncurry $ unsafeWrite a) $ zip [0 ..] lst
        pure a
    {-# INLINE fromList #-}

instance (MonadIO m, Storable.KnownConstantStaticSize a)
      => Grow m (SmallVector n a) where
    grow = \a -> do
        oldCapacity <- capacity a
        elemCount   <- size     a
        ptr         <- elemsPtr a
        let newSize       = oldCapacity * 2
            elemByteSize  = Storable.constantStaticSize @a
            bytesToMalloc = elemByteSize * newSize
            bytesToCopy   = elemByteSize * elemCount
            localMemPtr   = Struct.fieldPtr _localMem a
        newElemsPtr <- liftIO $ Mem.mallocBytes bytesToMalloc
        liftIO $ Mem.copyBytes newElemsPtr ptr bytesToCopy
        whenM (usesDynamicMemory a) $
            liftIO (Mem.free ptr)
        Struct.writeField _capacity a newSize
        Struct.writeField _offset a $! newElemsPtr `minusPtr` localMemPtr
    {-# INLINE grow #-}

instance (MonadIO m, Storable.StaticPoke View m a)
      => PushBack m (SmallVector n a) where
    pushBack = \a v -> do
        siz <- size     a
        cap <- capacity a
        when (siz == cap) $ grow a
        unsafeWrite a siz v
        Struct.writeField _length a $! siz + 1
    {-# INLINE pushBack #-}

instance
    ( MonadIO m
    , Grow  m (SmallVector n a)
    , Write m (SmallVector n a)
    , Storable.KnownConstantStaticSize a
    ) => InsertAt m (SmallVector n a) where
    insertAt = \a ix v -> do
        siz <- size     a
        cap <- capacity a
        when (siz == cap) $ grow a
        ptr0 <- elemsPtr a
        let elSize  = Storable.constantStaticSize @a
            ptrIx   = ptr0  `plusPtr` (elSize * ix)
            ptrIx'  = ptrIx `plusPtr` elSize
            byteOff = elSize * (siz - ix)
        when (byteOff > 0) $ liftIO $ Mem.moveBytes ptrIx' ptrIx byteOff
        unsafeWrite a ix v
        Struct.writeField _length a $! siz + 1
    {-# INLINE insertAt #-}

instance
    ( MonadIO m
    , Storable.KnownConstantStaticSize a
    ) => RemoveAt m (SmallVector n a) where
    removeAt = \a ix -> do
        siz  <- size a
        ptr0 <- elemsPtr a
        let elSize  = Storable.constantStaticSize @a
            ptrIx   = ptr0  `plusPtr` (elSize * ix)
            ptrIx'  = ptrIx `plusPtr` elSize
            byteOff = elSize * (siz - ix - 1)
        when (byteOff > 0) $ liftIO $ Mem.moveBytes ptrIx ptrIx' byteOff
        Struct.writeField _length a $! siz - 1
    {-# INLINE removeAt #-}



-- === Memory management instances === --

instance Applicative m
      => Storable.Peek View m (SmallVector n a) where
    peek = pure . coerce
    {-# INLINE peek #-}

instance (MonadIO m, Storable.KnownConstantStaticSize (SmallVector n a))
      => Storable.Poke View m (SmallVector n a) where
    poke = \ptr a ->
        let size = Storable.constantStaticSize @(SmallVector n a)
        in  liftIO $ Mem.copyBytes (coerce a) ptr size
    {-# INLINE poke #-}

instance MonadIO m => Data.ShallowDestructor1 m (SmallVector n) where
    destructShallow1 = free
    {-# INLINE destructShallow1 #-}


-- === Debug instances === --

instance (Show a, ToList IO (SmallVector n a))
      => Show (SmallVector n a) where
    show = show . unsafePerformIO . toList



-- === Deprecated instances === --

type instance Property.Get Dynamics (SmallVector n a) = Dynamic

instance
    ( Storable.Peek View IO a
    , Storable.KnownConstantStaticSize (SmallVector n a)
    , Storable View IO (SmallVector n a)
    , Type.KnownInt n
    ) => StdStorable.Storable (SmallVector n a) where
    sizeOf    = \ ~_ -> Storable.constantStaticSize @(SmallVector n a)
    alignment = \ ~_ -> StdStorable.alignment (undefined :: Int)
    peek      = Storable.peek @View
    poke      = Storable.poke @View
    {-# INLINE sizeOf #-}


-- WARNING: this instance is strange. It does not release self-memory,
--          because it is used for placement-new objects
instance MonadIO m
      => Data.Destructor1 m (SmallVector n) where
    destruct1 = \a -> liftIO $ do
        whenM (usesDynamicMemory a) $ do
            let localMemPtr = Struct.fieldPtr _localMem a
            offset <- Struct.readField _offset a
            Mem.free $ localMemPtr `plusPtr` offset
    {-# INLINE destruct1 #-}
