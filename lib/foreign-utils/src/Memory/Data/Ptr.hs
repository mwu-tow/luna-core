{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE UndecidableInstances #-}

module Memory.Data.Ptr where

import Prologue

import qualified Data.Convert2.Class       as Convert
import qualified Foreign.ForeignPtr        as Raw
import qualified Foreign.ForeignPtr.Unsafe as Raw
import qualified Foreign.Marshal.Alloc     as Mem
import qualified Foreign.Ptr               as Raw
import qualified GHC.ForeignPtr            as Raw
import qualified Memory.Data.Management    as Memory

import GHC.Base         (Int (I#), minusAddr#)
import System.IO.Unsafe (unsafePerformIO)


-----------------
-- === Ptr === --
-----------------

-- === Definition === -

newtype Ptr (t :: Memory.ManagementType) a = Ptr (PtrImpl t a)

type family PtrImpl (t :: Memory.ManagementType) :: Type -> Type where
    PtrImpl 'Memory.Managed   = Raw.ForeignPtr
    PtrImpl 'Memory.Unmanaged = Raw.Ptr


-- === Aliases === --

type SomePtr t        = Ptr t ()
type ManagedPtr       = Ptr 'Memory.Managed
type UnmanagedPtr     = Ptr 'Memory.Unmanaged
type SomeManagedPtr   = SomePtr 'Memory.Managed
type SomeUnmanagedPtr = SomePtr 'Memory.Unmanaged


-- === Conversions === --

instance (a ~ b, t ~ 'Memory.Unmanaged)
      => Convert.To (Ptr t a) (Raw.Ptr b) where
    to = wrap ; {-# INLINE to #-}

instance (a ~ b, t ~ 'Memory.Managed)
      => Convert.To (Ptr t a) (Raw.ForeignPtr b) where
    to = wrap ; {-# INLINE to #-}

coercePtr :: Ptr r a -> Ptr t b
coercePtr = unsafeCoerce
{-# INLINE coercePtr #-}


-- === Arithmetics === --

class PtrType t where
    plus          :: ∀ src tgt. Ptr t src -> Int -> Ptr t tgt
    minus         :: ∀ src tgt. Ptr t tgt -> Ptr t src -> Int
    nullPtr       :: ∀ a. Ptr t a
    withRawPtr    :: ∀ a b m. MonadIO m => Ptr t a -> (Raw.Ptr a -> m b) -> m b
    mallocBytesIO :: ∀ a. Int -> IO (Ptr t a)


instance PtrType 'Memory.Unmanaged where
    plus          = \ptr -> wrap . Raw.plusPtr (unwrap ptr)
    minus         = \tgt src -> Raw.minusPtr (unwrap tgt) (unwrap src)
    nullPtr       = wrap Raw.nullPtr
    withRawPtr    = \ptr f -> f (unwrap ptr)
    mallocBytesIO = fmap wrap . Mem.mallocBytes
    {-# INLINE plus          #-}
    {-# INLINE minus         #-}
    {-# INLINE nullPtr       #-}
    {-# INLINE withRawPtr    #-}
    {-# INLINE mallocBytesIO #-}

instance PtrType 'Memory.Managed where
    plus = \ptr -> wrap . Raw.plusForeignPtr (unwrap ptr)
    {-# INLINE plus #-}

    minus = \ (unwrap -> Raw.ForeignPtr !tgtAddr !_)
              (unwrap -> Raw.ForeignPtr !srcAddr !_)
            -> I# (minusAddr# tgtAddr srcAddr)
    {-# INLINE minus #-}

    nullPtr = wrap (coerce __foreignNullPtr)
    {-# INLINE   nullPtr #-}

    withRawPtr = \ptr f -> do
        let fptr = unwrap ptr
        out <- f $! Raw.unsafeForeignPtrToPtr fptr
        liftIO $ Raw.touchForeignPtr fptr
        pure out
    {-# INLINE withRawPtr #-}

    mallocBytesIO = fmap wrap . Raw.mallocForeignPtrBytes
    {-# INLINE mallocBytesIO #-}


-- class Plus  t where plus  :: ∀ src tgt. Ptr t src -> Int -> Ptr t tgt
-- class Minus t where minus :: ∀ src tgt. Ptr t tgt -> Ptr t src -> Int

-- instance Plus 'Memory.Unmanaged where
--     plus = \ptr -> wrap . Raw.plusPtr (unwrap ptr)
--     {-# INLINE plus #-}

-- instance Minus 'Memory.Unmanaged where
--     minus = \tgt src -> Raw.minusPtr (unwrap tgt) (unwrap src)
--     {-# INLINE minus #-}

-- instance Plus 'Memory.Managed where
--     plus = \ptr -> wrap . Raw.plusForeignPtr (unwrap ptr)
--     {-# INLINE plus #-}

-- instance Minus 'Memory.Managed where
--     minus = \ (unwrap -> Raw.ForeignPtr !tgtAddr !_)
--               (unwrap -> Raw.ForeignPtr !srcAddr !_)
--            -> I# (minusAddr# tgtAddr srcAddr)
--     {-# INLINE minus #-}


-- === NullPtr === --

-- class NullPtr t where
--     nullPtr :: ∀ a. Ptr t a

-- instance NullPtr 'Memory.Unmanaged where
--     nullPtr = wrap Raw.nullPtr
--     {-# INLINE nullPtr #-}

-- instance NullPtr 'Memory.Managed where
--     nullPtr = wrap (coerce __foreignNullPtr)
--     {-# INLINE nullPtr #-}

__foreignNullPtr :: Raw.ForeignPtr ()
__foreignNullPtr = unsafePerformIO $ Raw.newForeignPtr_ Raw.nullPtr
{-# NOINLINE __foreignNullPtr #-}


-- === WithRawPtr === --

-- class WithRawPtr t where
--     withRawPtr :: ∀ a b m. MonadIO m => Ptr t a -> (Raw.Ptr a -> m b) -> m b

-- instance WithRawPtr 'Memory.Unmanaged where
--     withRawPtr = \ptr f -> f (unwrap ptr)
--     {-# INLINE withRawPtr #-}

-- instance WithRawPtr 'Memory.Managed where
--     withRawPtr = \ptr f -> do
--         let fptr = unwrap ptr
--         out <- f $! Raw.unsafeForeignPtrToPtr fptr
--         liftIO $ Raw.touchForeignPtr fptr
--         pure out
--     {-# INLINE withRawPtr #-}


-- === Malloc === --

-- class Malloc t where
--     mallocBytesIO :: ∀ a. Int -> IO (Ptr t a)

-- instance Malloc 'Memory.Unmanaged where
--     mallocBytesIO = fmap wrap . Mem.mallocBytes
--     {-# INLINE mallocBytesIO #-}

-- instance Malloc 'Memory.Managed where
--     mallocBytesIO = fmap wrap . Raw.mallocForeignPtrBytes
--     {-# INLINE mallocBytesIO #-}

mallocBytes :: ∀ t m a. PtrType t => MonadIO m => Int -> m (Ptr t a)
mallocBytes = liftIO . mallocBytesIO
{-# INLINE mallocBytes #-}



-- === Instances === --

makeLenses ''Ptr
deriving instance Eq     (PtrImpl t a) => Eq     (Ptr t a)
deriving instance NFData (PtrImpl t a) => NFData (Ptr t a)
deriving instance Ord    (PtrImpl t a) => Ord    (Ptr t a)
deriving instance Show   (PtrImpl t a) => Show   (Ptr t a)
