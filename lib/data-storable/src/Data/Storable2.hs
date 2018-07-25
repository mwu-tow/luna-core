{-# LANGUAGE TypeInType           #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Storable2 where

import Prologue hiding (Generic, Lens, lens, read)

import qualified Data.Convert2.Class    as Convert
import qualified Foreign.ForeignPtr     as ForeignPtr
import qualified Foreign.Marshal.Alloc  as Mem
import qualified Foreign.Marshal.Utils  as Mem
import qualified Foreign.Ptr            as Ptr
import qualified Foreign.Storable.Class as Storable
import qualified GHC.Generics           as Generics
import qualified Memory                 as Memory
import qualified Type.Data.List         as List
import qualified Type.Data.Maybe        as Type
import qualified Type.Known             as Type

import Foreign.ForeignPtr        (ForeignPtr, plusForeignPtr, touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr               (plusPtr)
import Foreign.Storable.Class    (Storable)
import GHC.Generics              ((:*:))
import Type.Data.List            (type (<>))




data Fieldx




----------------------
-- === FieldRef === --
----------------------

-- === Definition === --

newtype FieldRef   t a    = FieldRef (Memory.Ptr t a)
type    FieldRefOf a      = FieldRef (Memory.Management a)
type    UnmanagedFieldRef = FieldRef 'Memory.Unmanaged
type    ManagedFieldRef   = FieldRef 'Memory.Managed
makeLenses ''FieldRef



------------------
-- === Path === --
------------------

-- === Definition === --

data Path (path :: [Type]) = Path

data FieldSig = FieldSig Type Type
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


-- === Fields === --

type family Fields    a :: [Type]
type family FieldIdx  field layout :: Maybe Nat
type family FieldType field layout :: Type


-- === Utils === --

type family ResolvePath (path :: t) a where
    ResolvePath path Imp = Imp
    ResolvePath path a   = ResolvePath__ path a

type family   ResolvePath__ (p :: k)  a
type instance ResolvePath__ p         a = FieldType p (Layout a)
type instance ResolvePath__ '[]       a = a
type instance ResolvePath__ (p ': ps) a = ResolvePath__ ps (ResolvePath__ p a)

type family LookupFieldType idx (fields :: [Type]) where
    LookupFieldType ('Just idx) fields = List.Index' idx fields

type FieldTypeByIdx f layout
   = LookupFieldType (FieldIdx f layout) (Fields layout)



----------------------
-- === Layouts === --
----------------------

-- === FieldList === --

data FieldList (layout :: [FieldSig])
type instance Fields (FieldList lst) = MapFieldSigType lst


-- === Generic === --

data Generic a
type GenericStruct t a = Struct t (Generic a)

type instance Fields (Generic a) = MapFieldSigType (GenericFieldSig__ a)
type GenericFieldSig__ a         = GenericFields__ (Generics.Rep a)

type family GenericFields__ (g :: Type -> Type) :: [FieldSig] where
    GenericFields__ (a :*: b)         = GenericFields__ a <> GenericFields__ b
    GenericFields__ (Generics.D1 _ a) = GenericFields__ a
    GenericFields__ (Generics.C1 _ a) = GenericFields__ a
    GenericFields__
        (Generics.S1 ('Generics.MetaSel ('Just name) _ _ _) (Generics.Rec0 a))
        = '[Label name -:: a]
    GenericFields__
        (Generics.S1 ('Generics.MetaSel 'Nothing _ _ _) (Generics.Rec0 a))
        = '[Unlabeled -:: a]

instance Storable.KnownConstantSize (Fields (Generic a))
      => Storable.KnownConstantSize (Generic a) where
    constantSize = Storable.constantSize @(Fields (Generic a))
    {-# INLINE constantSize #-}


type instance FieldType field (Generic a) = FieldTypeByIdx field (Generic a)




----------------------------
-- === FieldAccessors === --
----------------------------

-- === Idx === --

data Idx (idx :: Nat)
type instance FieldIdx (Idx idx) _ = 'Just idx


-- === Label === --

data Unlabeled
data Label (label :: Symbol)

type instance FieldIdx (Label s) (FieldList lst) = FieldListIdx (Label s) lst
type family   FieldListIdx s lst where
    FieldListIdx s ((s -:: v) ': _) = 'Just 0
    FieldListIdx s ((t -:: _) ': l) = Type.SuccMaybe (FieldListIdx s l)
    FieldListIdx _ _                = 'Nothing

type instance FieldIdx (Label s) (Generic a)
   = FieldIdx (Label s) (FieldList (GenericFieldSig__ a))



------------------
-- === Lens === --
------------------

-- === Definition === --

type Lens t = (∀ a. Path a -> Path (ToPath t <> a))

type family ToPath (s :: k) :: [Type] where
    ToPath (s :: Type)   = '[s]
    ToPath (s :: [Type]) = s


-- === Utils === --

type AppliedLens a = Path '[] -> Path a

lens :: ∀ s. Lens s
lens = \_ -> Path
{-# INLINE lens #-}

autoLens :: Path a -> Path b
autoLens = \_ -> Path
{-# INLINE autoLens #-}



--------------------
-- === Struct === --
--------------------

-- === Definition === --

type    Struct__ t (layout :: Type) = Memory.SomePtr t
newtype Struct   t (layout :: Type) = Struct (Struct__ t layout)


-- === Aliases === --

type ManagedStruct   = Struct 'Memory.Managed
type UnmanagedStruct = Struct 'Memory.Unmanaged


-- === Generalization === --

class IsStruct a where
    type Layout a :: Type
    struct :: Iso' a (Struct (Memory.Management a) (Layout a))

    type Layout a = Layout (Unwrapped a)
    default struct
        :: (Wrapped a, Unwrapped a ~ Struct (Memory.Management a) (Layout a))
        => Iso' a (Struct (Memory.Management a) (Layout a))
    struct = wrapped'
    {-# INLINE struct #-}

instance IsStruct (Struct t fields) where
    type Layout (Struct t fields) = fields
    struct = id
    {-# INLINE struct #-}


-- === Instances === --

makeLenses ''Struct

type instance Memory.Management (Struct t _) = t

deriving instance Eq     (Struct__ t fields) => Eq     (Struct t fields)
deriving instance NFData (Struct__ t fields) => NFData (Struct t fields)
deriving instance Ord    (Struct__ t fields) => Ord    (Struct t fields)
deriving instance Show   (Struct__ t fields) => Show   (Struct t fields)

instance Storable.KnownConstantSize (Fields layout)
      => Storable.KnownConstantSize (Struct t layout) where
    constantSize = Storable.constantSize @(Fields layout)
    {-# INLINE constantSize #-}



-----------------------------
-- === Reader / Writer === --
-----------------------------

-- === Definition === --

type  Modifier path a = (Reader path a, Writer path a)
class Reader   path a where readByTypeIO  :: a -> IO (ResolvePath path a)
class Writer   path a where writeByTypeIO :: a -> ResolvePath path a -> IO ()

type family FromPath p where FromPath (Path p) = p

-- === Types === --

type FieldRefPeek t a = (Memory.PtrType t, Storable.Peek Fieldx IO a)
type FieldRefPoke t a = (Memory.PtrType t, Storable.Poke Fieldx IO a)


-- === API === --

read :: ∀ path a m. (Reader path a, MonadIO m)
    => AppliedLens path -> a -> m (ResolvePath path a)
read = const $ readByType @path
{-# INLINE read #-}

write :: ∀ path a m. (Writer path a, MonadIO m)
    => AppliedLens path -> a -> ResolvePath path a -> m ()
write = const $ writeByType @path
{-# INLINE write #-}

readByType :: ∀ path a m. (Reader path a, MonadIO m)
    => a -> m (ResolvePath path a)
readByType = liftIO . readByTypeIO @path
{-# INLINE readByType #-}

writeByType :: ∀ path a m. (Writer path a, MonadIO m)
    => a -> ResolvePath path a -> m ()
writeByType = liftIO .: writeByTypeIO @path
{-# INLINE writeByType #-}

peekRef :: (FieldRefPeek t a, MonadIO m) => FieldRef t a -> m a
peekRef = \a -> liftIO $ Memory.withUnmanagedPtr (unwrap a)
                       $ Storable.peek @Fieldx
{-# INLINE peekRef #-}

pokeRef :: (FieldRefPoke t a, MonadIO m) => FieldRef t a -> a -> m ()
pokeRef = \a v -> liftIO $ Memory.withUnmanagedPtr (unwrap a)
                         $ flip (Storable.poke @Fieldx) v
{-# INLINE pokeRef #-}


-- === Instances === --

instance {-# OVERLAPPABLE #-}
    (Has field a, FieldRefPeek (Memory.Management a) (ResolvePath field a))
      => Reader field a where
    readByTypeIO = peekRef . fieldRef @field
    {-# INLINE readByTypeIO #-}

instance {-# OVERLAPPABLE #-}
    (Has field a, FieldRefPoke (Memory.Management a) (ResolvePath field a))
      => Writer field a where
    writeByTypeIO = pokeRef . fieldRef @field
    {-# INLINE writeByTypeIO #-}


-- === Early resolution block === --

instance {-# OVERLAPPABLE #-} Reader Imp   a   where readByTypeIO = impossible
instance {-# OVERLAPPABLE #-} Reader field Imp where readByTypeIO = impossible

instance {-# OVERLAPPABLE #-} Writer Imp   a   where writeByTypeIO = impossible
instance {-# OVERLAPPABLE #-} Writer field Imp where writeByTypeIO = impossible



-----------------
-- === Has === --
-----------------

-- === Definition === --

type  HasCtx a = (IsStruct a, Memory.PtrType (Memory.Management a))
class HasCtx a => Has path (a :: Type) where
    byteOffset :: Int


-- === Utils === --

fieldRef :: ∀ path a. Has path a => a -> FieldRefOf a (ResolvePath path a)
fieldRef = \a ->
    let ptr = Memory.coercePtr . unwrap $ a ^. struct
        off = byteOffset @path @a
    in FieldRef $! ptr `Memory.plus` off
{-# INLINE fieldRef #-}

ref :: ∀ path a. Has path a
    => AppliedLens path -> a -> FieldRefOf a (ResolvePath path a)
ref = const $ fieldRef @path
{-# INLINE ref #-}


-- === Instances ==== --

class HasField__
    (field  :: Type)
    (fields :: [Type])
    (layout :: Type)
    (idx    :: Maybe Nat) where
    byteOffset__ :: Int

instance HasCtx a => Has '[] a where
    byteOffset = 0
    {-# INLINE byteOffset #-}

instance
    ( layout ~ Layout a
    , idx    ~ FieldIdx field layout
    , HasCtx a
    , HasField__ field path layout idx
    ) => Has (field ': path) a where
    byteOffset = byteOffset__ @field @path @layout @idx
    {-# INLINE byteOffset #-}

instance {-# OVERLAPPABLE #-}
    ( fields     ~ Fields layout
    , prevFields ~ List.Take   idx fields
    , tgtType    ~ List.Index' idx fields
    , Storable.KnownConstantSize prevFields
    , Has path tgtType
    ) => HasField__ field path layout ('Just idx) where
    byteOffset__ = Storable.constantSize @prevFields
                 + byteOffset @path @tgtType
    {-# INLINE byteOffset__ #-}

instance
    ( fields     ~ Fields layout
    , prevFields ~ List.Take idx fields
    , Storable.KnownConstantSize prevFields
    ) => HasField__ field '[] layout ('Just idx) where
    byteOffset__ = Storable.constantSize @prevFields
    {-# INLINE byteOffset__ #-}



--------------------------
-- === Construction === --
--------------------------

-- === Definition === --

type ConstructorSig    a        = ConsSig__ a (Fields (Layout a))
type StructConstructor a layout = Cons__  a (Fields layout)
type Allocator  t               = IO (Memory.SomePtr t)
type Serializer t               = Memory.SomePtr t -> (IO (), Memory.SomePtr t)
type Constructor       a        = ( StructConstructor a (Layout a)
                                  , ConsTgt__ (ConstructorSig a) ~ a
                                  )

placementWith :: ∀ a. Constructor a
    => Allocator (Memory.Management a) -> ConstructorSig a
placementWith = \alloc -> cons__ @a @(Fields (Layout a))
                          alloc (\ptr -> (pure (), ptr))
{-# INLINE placementWith #-}

placementNew :: ∀ a. Constructor a
    => Memory.Ptr (Memory.Management a) a -> ConstructorSig a
placementNew = placementWith . pure . Memory.coercePtr
{-# INLINE placementNew #-}

construct :: ∀ a.
    ( Constructor a
    , Memory.PtrType (Memory.Management a)
    , Storable.KnownConstantSize (Layout a)
    ) => ConstructorSig a
construct = placementWith @a (Memory.mallocBytes size) where
    size = Storable.constantSize @(Layout a)
{-# INLINE construct #-}

free :: ∀ a m. (IsStruct a, MonadIO m, Memory.AssertUnmanaged a) => a -> m ()
free = liftIO . Mem.free . unwrap . unwrap . view struct
{-# INLINE free #-}

unsafeCastFromPtr :: ∀ a p. IsStruct a
    => Memory.Ptr (Memory.Management a) a -> a
unsafeCastFromPtr = view (from struct)
    . Struct @(Memory.Management a) @(Layout a) . Memory.coercePtr
{-# INLINE unsafeCastFromPtr #-}


-- === Internal === --

type family ConsSig__ a types where
    ConsSig__ a (t ': ts) = t -> (ConsSig__ a ts)
    ConsSig__ a '[]       = IO a

type family ConsTgt__ f where
    ConsTgt__ (_ -> a) = ConsTgt__ a
    ConsTgt__ (IO a)   = a

class Cons__ a types where
    cons__ :: Allocator  (Memory.Management a)
           -> Serializer (Memory.Management a)
           -> ConsSig__ a types

instance {-# OVERLAPPABLE #-}
    ( Cons__ a fs
    , Storable.KnownConstantSize f
    , Storable.Poke Fieldx IO f
    , Memory.PtrType (Memory.Management a)
    ) => Cons__ a (f ': fs) where
    cons__ = \alloc f a -> cons__ @a @fs alloc $ \ptr ->
        let (!m, !ptr') = f ptr
            f'          = m >> Memory.withUnmanagedRawPtr ptr'
                               (flip (Storable.poke @Fieldx) a . coerce)
            ptr''       = ptr' `Memory.plus` Storable.constantSize @f
        in  (f', ptr'')
    {-# INLINE cons__ #-}

instance
    ( Cons__ a '[]
    , Storable.Poke Fieldx IO f
    , Memory.PtrType (Memory.Management a)
    ) => Cons__ a '[f] where
    cons__ = \alloc f a -> cons__ @a @('[]) alloc $ \ptr ->
        let (!m, !ptr') = f ptr
            f'          = m >> Memory.withUnmanagedRawPtr ptr'
                               (flip (Storable.poke @Fieldx) a . coerce)
        in  (f', ptr')
    {-# INLINE cons__ #-}

instance IsStruct a
      => Cons__ a '[] where
    cons__ = \alloc f -> do
        ptr <- alloc
        let (!m, !_) = f ptr
        (Struct ptr ^. from struct) <$ m
    {-# INLINE cons__ #-}










-- type instance Storable.KnownConstantSize (FieldList lst)
--    = Storable.KnownConstantSize lst

instance Storable.KnownConstantSize lst
      => Storable.KnownConstantSize (FieldList lst) where
    constantSize = Storable.constantSize @lst
    {-# INLINE constantSize #-}

type Foo = Lens "foo"
type Bar = Lens "bar"
type Baz = Lens "baz"

newtype X = X (Struct 'Memory.Unmanaged (FieldList '[Label "foo" -:: Int, Label "bar" -:: Int, Label "baz" -:: Int]))
makeLenses ''X
type instance Memory.Management X = 'Memory.Unmanaged
instance IsStruct X


data YD = YD
    { yfoo :: Int
    , ybar :: Int
    , ybaz :: Int
    } deriving (Generics.Generic)

newtype Y = Y (Struct 'Memory.Unmanaged (Generic YD))
makeLenses ''Y
type instance Memory.Management Y = 'Memory.Unmanaged
instance IsStruct Y


foo :: Lens (Label "yfoo")
foo = autoLens

bar :: Lens (Label "ybar")
bar = autoLens

baz :: Lens (Label "ybaz")
baz = autoLens

xxx :: Lens '[Label "yfoo", Label "ybar"]
xxx = foo . bar

-- -- -- Struct.Has (Lens "foo") a

-- tst :: a -> _
-- tst a = read xxx a

xcons :: Int -> Int -> Int -> IO X
xcons = construct @X

ycons :: Int -> Int -> Int -> IO Y
ycons = construct @Y

main :: IO ()
main = do
    a <- ycons 1 2 3
    print =<< read foo a
    print =<< read bar a
    print =<< read baz a
    write foo a 10
    print =<< read foo a
    print =<< read (autoLens :: Lens (Idx 1)) a

    print "end"




-- newtype Y = Y (Sum '[A, B, C])


-- data S
--     = A X
--     | B Y
    -- | C Z



