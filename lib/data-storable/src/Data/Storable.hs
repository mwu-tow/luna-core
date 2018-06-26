{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Storable where

import Prologue hiding (product)

import qualified Data.Convert2.Class    as Convert
import qualified Foreign.ForeignPtr     as ForeignPtr
import qualified Foreign.Marshal.Alloc  as Mem
import qualified Foreign.Marshal.Utils  as Mem
import qualified Foreign.Ptr            as Ptr
import qualified Foreign.Storable.Class as Storable
import qualified Memory                 as Memory
import qualified Type.Data.List         as List
import qualified Type.Known             as Type

import Foreign.ForeignPtr        (ForeignPtr, plusForeignPtr, touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr               (plusPtr)
import Foreign.Storable.Class    (Storable)
import Type.Data.Semigroup       (type (<>))




-------------------
-- === Field === --
-------------------

-- === Field reference === --

data Field
data FieldRef (name :: Symbol) = FieldRef deriving (Show)

field :: ∀ name. FieldRef name
field = FieldRef
{-# INLINE field #-}


-- === Field signature === --

data FieldSig = FieldSig Symbol Type
type name -:: tp = 'FieldSig name tp

type family FieldSigType field where FieldSigType ('FieldSig _ t) = t
type family FieldSigName field where FieldSigName ('FieldSig n _) = n

type family MapFieldSigType fields where
    MapFieldSigType (f ': fs) = FieldSigType f ': MapFieldSigType fs
    MapFieldSigType '[]       = '[]

type family MapFieldSigName fields where
    MapFieldSigName (f ': fs) = FieldSigName f ': MapFieldSigName fs
    MapFieldSigName '[]       = '[]

instance Storable.KnownConstantSize a
      => Storable.KnownConstantSize ('FieldSig n a) where
    constantSize = Storable.constantSize @a
    {-# INLINE constantSize #-}


-- === FieldType === --

type FieldType name a = LookupFieldType name (Fields a)
type LookupFieldType name fields = LookupFieldType__ name fields

type family LookupFieldType__ name fields where
    LookupFieldType__ n (('FieldSig n v) ': _) = v
    LookupFieldType__ n (_ ': fs)              = LookupFieldType__ n fs



--------------------
-- === Struct === --
--------------------

-- === Definition === --

type    Struct__ t (fields :: [FieldSig]) = Memory.SomePtr t
newtype Struct   t (fields :: [FieldSig]) = Struct (Struct__ t fields)


-- === Aliases === --

type ManagedStruct   = Struct 'Memory.Managed
type UnmanagedStruct = Struct 'Memory.Unmanaged


-- === Generalization === --

class IsStruct a where
    type Fields a :: [FieldSig]
    struct :: Iso' a (Struct (Memory.Management a) (Fields a))

    type Fields a = Fields (Unwrapped a)
    default struct :: (Wrapped a, Unwrapped a ~ Struct (Memory.Management a) (Fields a))
                   => Iso' a (Struct (Memory.Management a) (Fields a))
    struct = wrapped' ; {-# INLINE struct #-}

instance IsStruct (Struct t fields) where
    type Fields (Struct t fields) = fields
    struct = id ; {-# INLINE struct #-}


-- === Instances === --

makeLenses ''Struct

type instance Memory.Management (Struct t _) = t

instance Storable.KnownConstantSize (MapFieldSigType fields)
      => Storable.KnownConstantSize (Struct t fields) where
    constantSize = Storable.constantSize @(MapFieldSigType fields)
    {-# INLINE constantSize #-}

deriving instance Eq     (Struct__ t fields) => Eq     (Struct t fields)
deriving instance NFData (Struct__ t fields) => NFData (Struct t fields)
deriving instance Ord    (Struct__ t fields) => Ord    (Struct t fields)
deriving instance Show   (Struct__ t fields) => Show   (Struct t fields)



-----------------------------------
-- === Field Reader / Writer === --
-----------------------------------

-- === API === --

fieldPtr :: ∀ name a. HasField name a
         => FieldRef name -> a -> Memory.Ptr (Memory.Management a) (FieldType name a)
fieldPtr = \_ -> fieldPtrByName @name
{-# INLINE fieldPtr #-}

readField :: ∀ name a m. (FieldReader name a, MonadIO m)
          => FieldRef name -> a -> m (FieldType name a)
readField = \_ -> liftIO . readFieldByNameIO @name
{-# INLINE readField #-}

writeField :: ∀ name a m. (FieldWriter name a, MonadIO m)
    => FieldRef name -> a -> FieldType name a -> m ()
writeField = \_ -> liftIO .: writeFieldByNameIO @name
{-# INLINE writeField #-}

modifyField :: ∀ name t a m. (FieldEditor name a, MonadIO m)
    => FieldRef name -> (FieldType name a -> m (t, FieldType name a)) -> a -> m t
modifyField = \field f a -> do
    v <- readField field a
    (!t, !v') <- f v
    writeField field a v'
    pure t
{-# INLINE modifyField #-}

modifyField_ :: ∀ name t a m. (FieldEditor name a, MonadIO m)
    => FieldRef name -> (FieldType name a -> m (FieldType name a)) -> a -> m ()
modifyField_ = \field f a -> do
    v  <- readField field a
    v' <- f v
    writeField field a v'
{-# INLINE modifyField_ #-}


-- === Class === --

type FieldEditor name a = (FieldReader name a, FieldWriter name a)

class HasField (name :: Symbol) a where
    fieldPtrByName :: a -> Memory.Ptr (Memory.Management a) (FieldType name a)

class FieldReader (name :: Symbol) a where
    readFieldByNameIO :: a -> IO (FieldType name a)

class FieldWriter (name :: Symbol) a where
    writeFieldByNameIO :: a -> FieldType name a -> IO ()


-- === Internal === --

class HasField__ t (name :: Symbol) (fs :: [FieldSig]) (idx :: Maybe Nat) where
    fieldPtr__ :: Struct t fs -> Memory.Ptr t (LookupFieldType name fs)

instance
    ( fields' ~ List.Take idx (MapFieldSigType fields)
    , Storable.KnownConstantSize fields'
    , Memory.PtrType t
    ) => HasField__ t name fields ('Just idx) where
    fieldPtr__ = \(Struct !ptr) ->
        let off = Storable.constantSize @fields'
        in  ptr `Memory.plus` off
    {-# INLINE fieldPtr__ #-}

instance (HasField name a, Storable.Peek Field IO (FieldType name a), Memory.PtrType (Memory.Management a))
      => FieldReader name a where
    readFieldByNameIO = \a -> Memory.withUnmanagedRawPtr (fieldPtrByName @name a) $ Storable.peek @Field
    {-# INLINE readFieldByNameIO #-}

instance (HasField name a, Storable.Poke Field IO (FieldType name a), Memory.PtrType (Memory.Management a))
      => FieldWriter name a where
    writeFieldByNameIO = \a val -> Memory.withUnmanagedRawPtr (fieldPtrByName @name a) $ flip (Storable.poke @Field) val
    {-# INLINE writeFieldByNameIO #-}

instance
    ( fields ~ Fields a
    , idx    ~ List.ElemIndex name (MapFieldSigName fields)
    , man    ~ Memory.Management a
    , IsStruct a
    , HasField__ man name fields idx
    ) => HasField name a where
    fieldPtrByName = fieldPtr__ @man @name @fields @idx . view struct
    {-# INLINE fieldPtrByName #-}




--------------------------
-- === Construction === --
--------------------------

-- === Definition === --

type ConstructorSig    a        = ConsSig__ a (MapFieldSigType (Fields a))
type StructConstructor a fields = Cons__  a (MapFieldSigType fields)
type Allocator  t               = IO (Memory.SomePtr t)
type Serializer t               = Memory.SomePtr t -> (IO (), Memory.SomePtr t)
type Constructor       a        = ( StructConstructor a (Fields a)
                                  , ConsTgt__ (ConstructorSig a) ~ a
                                  )

placementWith :: ∀ a. Constructor a => Allocator (Memory.Management a) -> ConstructorSig a
placementWith = \alloc -> cons__ @a @(MapFieldSigType (Fields a))
                          alloc (\ptr -> (pure (), ptr))
{-# INLINE placementWith #-}

placementNew :: ∀ a. Constructor a => Memory.Ptr (Memory.Management a) a -> ConstructorSig a
placementNew = placementWith . pure . Memory.coercePtr
{-# INLINE placementNew #-}

construct :: ∀ a.
    ( Constructor a
    , Memory.PtrType (Memory.Management a)
    , Storable.KnownConstantSize (Fields a)
    ) => ConstructorSig a
construct = placementWith @a (Memory.mallocBytes size) where
    size = Storable.constantSize @(Fields a)
{-# INLINE construct #-}

free :: ∀ a m. (IsStruct a, MonadIO m, Memory.AssertUnmanaged a) => a -> m ()
free = liftIO . Mem.free . unwrap . unwrap . view struct
{-# INLINE free #-}

unsafeCastFromPtr :: ∀ a p. IsStruct a => Memory.Ptr (Memory.Management a) () -> a
unsafeCastFromPtr = view (from struct) . Struct @(Memory.Management a) @(Fields a)
{-# INLINE unsafeCastFromPtr #-}


-- === Internal === --

type family ConsSig__ a types where
    ConsSig__ a (t ': ts) = t -> (ConsSig__ a ts)
    ConsSig__ a '[]       = IO a

type family ConsTgt__ f where
    ConsTgt__ (_ -> a) = ConsTgt__ a
    ConsTgt__ (IO a)   = a

class Cons__ a types where
    cons__ :: Allocator (Memory.Management a) -> Serializer (Memory.Management a) -> ConsSig__ a types

instance {-# OVERLAPPABLE #-}
    ( Cons__ a fs
    , Storable.KnownConstantSize f
    , Storable.Poke Field IO f
    , Memory.PtrType (Memory.Management a)
    ) => Cons__ a (f ': fs) where
    cons__ = \alloc f a -> cons__ @a @fs alloc $ \ptr ->
        let (!m, !ptr') = f ptr
            f'          = m >> Memory.withUnmanagedRawPtr ptr' (flip (Storable.poke @Field) a . coerce)
            ptr''       = ptr' `Memory.plus` Storable.constantSize @f
        in  (f', ptr'')
    {-# INLINE cons__ #-}

instance
    ( Cons__ a '[]
    , Storable.Poke Field IO f
    , Memory.PtrType (Memory.Management a)
    ) => Cons__ a '[f] where
    cons__ = \alloc f a -> cons__ @a @('[]) alloc $ \ptr ->
        let (!m, !ptr') = f ptr
            f'          = m >> Memory.withUnmanagedRawPtr ptr' (flip (Storable.poke @Field) a . coerce)
        in  (f', ptr')
    {-# INLINE cons__ #-}

instance IsStruct a
      => Cons__ a '[] where
    cons__ = \alloc f -> do
        ptr <- alloc
        let (!m, !_) = f ptr
        (Struct ptr ^. from struct) <$ m
    {-# INLINE cons__ #-}







newtype X = X (Struct 'Memory.Unmanaged '["foo" -:: Int, "bar" -:: Int, "baz" -:: Int])
makeLenses ''X

type instance Memory.Management X = 'Memory.Unmanaged

instance IsStruct X

foo :: FieldRef "foo"
bar :: FieldRef "bar"
baz :: FieldRef "baz"
foo = field
bar = field
baz = field

test :: Int -> Int -> Int -> IO X
test = construct @X

main :: IO ()
main = do
    a <- test 1 2 3
    print =<< readField foo a
    print =<< readField bar a
    print =<< readField baz a
    writeField foo a 10
    print =<< readField foo a

    print "end"


-- construct :: ∀ fields. Constructor fields
--           => MemAllocator -> ConstructorSig fields
-- construct malloc = allocProduct @fields malloc (\ptr -> (pure (), ptr))
-- {-# INLINE construct #-}

-- var :: Int -> IO Varx
-- var = product @VarLayout Mem.mallocBytes
-- {-# NOINLINE var #-}

-- match :: Memory.SomePtr -> Vector Memory.SomePtr -> IO Matchx
-- match = product @MatchLayout Mem.mallocBytes
-- {-# NOINLINE match #-}

-- main :: IO ()
-- main = do
--     v <- var 7
--     print =<< viewField @"name" v
--     -- print $ Type.val' @(ByteSize Varx)
--     print "end"
-- -- instance Storable (Struct layout)

-- -- data Model



-- -- type Node = Struct '[] '[Model]

