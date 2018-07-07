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


-- === FieldType === --

type family FieldIdx key layout :: Maybe Nat
type family Fields   a :: [Type]

type family FieldType path a where
    FieldType path Imp                = Imp
    FieldType (Path '[])       a = a
    FieldType (Path (f ': fs)) a = FieldType (Path fs)
        (LookupFieldType (FieldIdx f (Layout a)) (Fields (Layout a)))

type family LookupFieldType idx (fields :: [Type]) where
    LookupFieldType ('Just idx) fields = List.Index' idx fields



-----------------------
-- === FieldList === --
-----------------------

-- === Definition === --

data FieldList (layout :: [FieldSig])


-- === Instances === --

type instance Fields (FieldList lst) = MapFieldSigType lst



data Generic a

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

type instance FieldIdx (Label s) (Generic a) = FieldIdx (Label s) (FieldList (GenericFieldSig__ a))


------------------
-- === Lens === --
------------------

-- === Definition === --

type Lens t = (∀ a. Path a -> Path (ToPath t <> a))

type family ToPath (s :: k) :: [Type] where
    ToPath (s :: Type)   = '[s]
    ToPath (s :: [Type]) = s


-- === Utils === --

type AppliedLens a = Path '[] -> a

lens :: ∀ s. Lens s
lens = \_ -> Path
{-# INLINE lens #-}

autoLens :: Path a -> Path b
autoLens = \_ -> Path
{-# INLINE autoLens #-}



---------------------
-- === Product === --
---------------------

-- === Definition === --

type    Product__ t (layout :: Type) = Memory.SomePtr t
newtype Product   t (layout :: Type) = Product (Product__ t layout)


-- === Aliases === --

type ManagedStruct   = Product 'Memory.Managed
type UnmanagedStruct = Product 'Memory.Unmanaged


-- === Generalization === --

class IsStruct a where
    type Layout a :: Type
    struct :: Iso' a (Product (Memory.Management a) (Layout a))

    type Layout a = Layout (Unwrapped a)
    default struct
        :: (Wrapped a, Unwrapped a ~ Product (Memory.Management a) (Layout a))
        => Iso' a (Product (Memory.Management a) (Layout a))
    struct = wrapped'
    {-# INLINE struct #-}

instance IsStruct (Product t fields) where
    type Layout (Product t fields) = fields
    struct = id
    {-# INLINE struct #-}


-- === Instances === --

makeLenses ''Product

type instance Memory.Management (Product t _) = t

-- instance Storable.KnownConstantSize (MapFieldSigType fields)
--       => Storable.KnownConstantSize (Product t fields) where
--     constantSize = Storable.constantSize @(MapFieldSigType fields)
--     {-# INLINE constantSize #-}

deriving instance Eq     (Product__ t fields) => Eq     (Product t fields)
deriving instance NFData (Product__ t fields) => NFData (Product t fields)
deriving instance Ord    (Product__ t fields) => Ord    (Product t fields)
deriving instance Show   (Product__ t fields) => Show   (Product t fields)



-----------------------------
-- === Reader / Writer === --
-----------------------------

-- === Definition === --

type Editor field a = (Reader field a, Writer field a)

class Reader (lens :: Type) (a :: Type) where
    readFieldIO :: a -> IO (FieldType lens a)

class Writer (field :: Type) (a :: Type) where
    writeFieldIO :: a -> FieldType field a -> IO ()


-- === Types === --

type FieldRefPeek t a = (Memory.PtrType t, Storable.Peek Fieldx IO a)
type FieldRefPoke t a = (Memory.PtrType t, Storable.Poke Fieldx IO a)


-- === API === --

read :: ∀ field a m. (Reader field a, MonadIO m)
     => AppliedLens field -> a -> m (FieldType field a)
read = const $ readField @field
{-# INLINE read #-}

write :: ∀ field a m. (Writer field a, MonadIO m)
    => AppliedLens field -> a -> FieldType field a -> m ()
write = const $ writeField @field
{-# INLINE write #-}

readField :: ∀ lens a m. (Reader lens a, MonadIO m)
    => a -> m (FieldType lens a)
readField = liftIO . readFieldIO @lens
{-# INLINE readField #-}

writeField :: ∀ field a m. (Writer field a, MonadIO m)
    => a -> FieldType field a -> m ()
writeField = liftIO .: writeFieldIO @field
{-# INLINE writeField #-}

peekRef :: (FieldRefPeek t a, MonadIO m) => FieldRef t a -> m a
peekRef = \a -> liftIO $ Memory.withUnmanagedPtr (unwrap a)
                       $ Storable.peek @Fieldx
{-# INLINE peekRef #-}

pokeRef :: (FieldRefPoke t a, MonadIO m) => FieldRef t a -> a -> m ()
pokeRef = \a v -> liftIO $ Memory.withUnmanagedPtr (unwrap a)
                         $ flip (Storable.poke @Fieldx) v
{-# INLINE pokeRef #-}


-- === Instances === --

instance (Has field a, FieldRefPeek (Memory.Management a) (FieldType field a))
      => Reader field a where
    readFieldIO = peekRef . fieldRef @field
    {-# INLINE readFieldIO #-}

instance (Has field a, FieldRefPoke (Memory.Management a) (FieldType field a))
      => Writer field a where
    writeFieldIO = pokeRef . fieldRef @field
    {-# INLINE writeFieldIO #-}


-- === Early resolution block === --

type ImpField = Path '[Imp]
instance {-# OVERLAPPABLE #-} Reader ImpField a   where readFieldIO = impossible
instance {-# OVERLAPPABLE #-} Reader field    Imp where readFieldIO = impossible



-----------------
-- === Has === --
-----------------

-- === Definition === --

type  HasCtx a = (IsStruct a, Memory.PtrType (Memory.Management a))
class HasCtx a => Has (field :: Type) (a :: Type) where
    byteOffset :: Int


-- === Utils === --

fieldRef :: ∀ field a. Has field a => a -> FieldRefOf a (FieldType field a)
fieldRef = \a ->
    let ptr = Memory.coercePtr . unwrap $ a ^. struct
        off = byteOffset @field @a
    in FieldRef $! ptr `Memory.plus` off
{-# INLINE fieldRef #-}

ref :: ∀ field a. Has field a
    => AppliedLens field -> a -> FieldRefOf a (FieldType field a)
ref = const $ fieldRef @field
{-# INLINE ref #-}


-- === Instances ==== --



class HasField__
    (field  :: Type)
    (fields :: [Type])
    (layout :: Type)
    (idx    :: Maybe Nat) where
    byteOffset__ :: Int

instance HasCtx a => Has (Path '[]) a where
    byteOffset = 0
    {-# INLINE byteOffset #-}

instance
    ( layout ~ Layout a
    , idx    ~ FieldIdx field layout
    , HasCtx a
    , HasField__ field fields layout idx
    ) => Has (Path (field ': fields)) a where
    byteOffset = byteOffset__ @field @fields @layout @idx
    {-# INLINE byteOffset #-}

instance {-# OVERLAPPABLE #-}
    ( types    ~ Fields layout
    , types'   ~ List.Take   idx types
    , tgtType  ~ List.Index' idx types
    , tgtField ~ Path fields
    , Storable.KnownConstantSize types'
    , Has tgtField tgtType
    ) => HasField__ field fields layout ('Just idx) where
    byteOffset__ = Storable.constantSize @types'
                 + byteOffset @tgtField @tgtType
    {-# INLINE byteOffset__ #-}

instance
    ( types  ~ Fields layout
    , types' ~ List.Take idx types
    , Storable.KnownConstantSize types'
    ) => HasField__ field '[] layout ('Just idx) where
    byteOffset__ = Storable.constantSize @types'
    {-# INLINE byteOffset__ #-}



------------------------
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
    . Product @(Memory.Management a) @(Layout a) . Memory.coercePtr
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
        (Product ptr ^. from struct) <$ m
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

newtype X = X (Product 'Memory.Unmanaged (FieldList '[Label "foo" -:: Int, Label "bar" -:: Int, Label "baz" -:: Int]))
makeLenses ''X
type instance Memory.Management X = 'Memory.Unmanaged
instance IsStruct X


data YD = YD
    { yfoo :: Int
    , ybar :: Int
    , ybaz :: Int
    } deriving (Generics.Generic)

newtype Y = Y (Product 'Memory.Unmanaged (Generic YD))
makeLenses ''Y
type instance Memory.Management Y = 'Memory.Unmanaged
instance IsStruct Y


foo :: Lens (Label "yfoo")
foo = autoLens

bar :: Lens (Label "ybar")
bar = autoLens

baz :: Lens (Label "ybaz")
baz = autoLens

xxx :: Lens '[Label "yfoo", Label "ysbar"]
xxx = foo . bar

-- -- -- Product.Has (Lens "foo") a

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
--     | C Z



