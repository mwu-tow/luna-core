{-# LANGUAGE UndecidableInstances #-}

module Data.Graph.Store.Buffer where

import Prologue hiding (Data)

import qualified Control.Monad.State.Layered     as State
import qualified Data.ByteString.Internal        as ByteString
import qualified Data.Convert2                   as Convert
import qualified Data.Convert2                   as Convert
import qualified Data.Graph.Data.Component.Class as Component
import qualified Data.Graph.Data.Component.List  as ComponentList
import qualified Data.Graph.Data.Component.List  as ComponentList
import qualified Data.Graph.Data.Graph.Class     as Graph
import qualified Data.Graph.Data.Layer.Class     as Layer
import qualified Data.Graph.Fold.Partition       as Partition
import qualified Data.Graph.Store.Size.Class     as Size
import qualified Data.Map.Strict                 as Map
import qualified Data.Mutable.Class              as Mutable
import qualified Data.Mutable.Plain              as Data
import qualified Data.Mutable.Storable.Array     as Array
import qualified Data.Storable                   as Struct
import qualified Foreign.ForeignPtr              as ForeignPtr
import qualified Foreign.Storable.Class          as Storable
import qualified Memory                          as Memory
import qualified Type.Data.List                  as List

import qualified Data.Graph.Fold.Class     as Fold
import qualified Data.Graph.Fold.Filter    as Fold
import qualified Data.Graph.Fold.Scoped    as Fold
import qualified Data.Graph.Fold.ScopedMap as Fold
import qualified Data.Graph.Fold.Struct    as Fold

import Data.ByteString                       (ByteString)
import Data.Graph.Data.Component.Class       (Component)
import Data.Graph.Data.Component.Set         (ComponentSet, ComponentSetA)
import Data.Graph.Data.Component.Vector      (ComponentVector, ComponentVectorA)
import Data.Graph.Store.Size.Class           (Size)
import Data.Map.Strict                       (Map)
import Data.Mutable.Storable.Array           (ManagedArray)
import Data.Mutable.Storable.SmallAutoVector (SmallVectorA)
import Data.Storable                         (type (-::), ManagedStruct)
import Foreign.ForeignPtr                    (touchForeignPtr)
import Foreign.ForeignPtr.Unsafe             (unsafeForeignPtrToPtr)
import Foreign.ForeignPtr.Utils              (SomeForeignPtr)
import Foreign.Ptr.Utils                     (SomePtr)
import Type.Data.Semigroup                   (type (<>))

import qualified Type.Show as Type


type RedirectMap = Map Memory.SomeUnmanagedPtr Memory.SomeUnmanagedPtr


------------------------------
-- === DynamicMemRegion === --
------------------------------

-- === Definition === --

newtype UnknownSizeMemRegion t a = UnknownSizeMemRegion (Memory.Ptr t a)
makeLenses ''UnknownSizeMemRegion

unsafeNull :: Memory.PtrType t => UnknownSizeMemRegion t a
unsafeNull = UnknownSizeMemRegion Memory.nullPtr
{-# INLINE unsafeNull #-}

-- FIXME: change it to Struct.FieldInitializer
instance Applicative m
      => Storable.Poke Struct.Field m (UnknownSizeMemRegion t a) where
    poke = \_ _ -> pure ()
    {-# INLINE poke #-}


type instance Item (UnknownSizeMemRegion t a) = a

deriving instance Show (Memory.Ptr t a) => Show (UnknownSizeMemRegion t a)

instance (Memory.PtrType t, Storable.Peek Storable.View m a, MonadIO m, Storable.KnownConstantSize a)
      => Mutable.Read m (UnknownSizeMemRegion t a) where
    unsafeRead = \a ix -> Memory.withUnmanagedRawPtr (unwrap a)
        $ \p -> Storable.peekElemOff @Storable.View p ix
    {-# INLINE unsafeRead #-}

instance (Memory.PtrType t, Storable.Poke Storable.View m a, MonadIO m, Storable.KnownConstantSize a)
      => Mutable.Write m (UnknownSizeMemRegion t a) where
    unsafeWrite = \a ix v -> Memory.withUnmanagedRawPtr (unwrap a)
        $ \p -> Storable.pokeElemOff @Storable.View p ix v
    {-# INLINE unsafeWrite #-}











-------------------------------
-- === StoreDynAllocator === --
-------------------------------

-- === Definition === --

data StoreDyn
type StoreDynAllocator = 'Memory.Allocator StoreDyn

newtype StoreDynState = StoreDynState (Memory.UnmanagedPtr ())
makeLenses ''StoreDynState

instance (State.Monad StoreDynState m, Storable.KnownConstantSize a)
      => Memory.Allocation StoreDynAllocator 'Memory.Unmanaged a m where
    allocate n = do
        ptr <- unwrap <$> State.get @StoreDynState
        State.put @StoreDynState $ wrap $ ptr `Memory.plus` (n * Storable.constantSize @a)
        pure (coerce ptr)


--------------------------------
-- === CopyInitialization === --
--------------------------------

-- === Definition === --

data CopyInitialization
type instance Fold.Result     CopyInitialization = ()
type instance Fold.LayerScope CopyInitialization = 'Fold.All

instance Monad m => Fold.ComponentBuilder CopyInitialization m comp

instance (Monad m, CopyInitializerP1 m (Layer.Cons layer))
      => Fold.LayerBuilder CopyInitialization m layer where
        layerBuild = \layer _ -> () <$ copyInitializeP1 layer


-- === CopyInitializerP1 === --

class Applicative m => CopyInitializerP1 m a where
    copyInitializeP1 :: ∀ t1. a t1 -> m (a t1)
    copyInitializeP1 = pure
    {-# INLINE copyInitializeP1 #-}

instance (Data.CopyInitializer1 m (ComponentSetA StoreDynAllocator comp), Applicative m)
      => CopyInitializerP1 m (ComponentSet comp) where
    copyInitializeP1 = \a -> a <$ Data.copyInitialize1 (Memory.setAllocator @StoreDynAllocator a)

instance Applicative m => CopyInitializerP1 m (Component comp)

instance {-# OVERLAPPABLE #-}
    ( Fold.Builder1 (Fold.Struct CopyInitialization) m a
    , Applicative m
    ) => CopyInitializerP1 m a where
    copyInitializeP1 = \a -> a <$ (Fold.build1 @(Fold.Struct CopyInitialization) a (pure ()))


-- === CopyInitialization Folds === --

instance (Data.CopyInitializer1 m (ComponentSetA StoreDynAllocator comp), Monad m)
      => Fold.Builder1 CopyInitialization m (ComponentSet comp) where
    build1 = \a x -> x <* Data.copyInitialize1 (Memory.setAllocator @StoreDynAllocator a)
    {-# INLINE build1 #-}

instance (Data.CopyInitializer1 m (ComponentVectorA StoreDynAllocator comp), Monad m)
      => Fold.Builder1 CopyInitialization m (ComponentVector comp) where
    build1 = \a x -> x <* Data.copyInitialize1 (Memory.setAllocator @StoreDynAllocator a)
    {-# INLINE build1 #-}

instance (Data.CopyInitializer m (SmallVectorA StoreDynAllocator comp a), Monad m)
      => Fold.Builder CopyInitialization m (SmallVectorA alloc comp a) where
    build = \a x -> x <* Data.copyInitialize (Memory.setAllocator @StoreDynAllocator a)
    {-# INLINE build #-}

instance Monad m => Fold.Builder1 CopyInitialization m (Component comp)



-----------------------------
-- === CopyInitializer === --
-----------------------------

-- === Definition === --

type ComponentStaticInitializer m =
    ( ComponentStaticInitializer__ (Graph.ComponentsM m) (State.StateT StoreDynState m)
    , Graph.KnownComponentNumber (Graph.Discover m)
    , MonadIO m
    )

copyInitializeComponents :: ∀ m. ComponentStaticInitializer m
    => [Int] -> BufferM m -> m ()
copyInitializeComponents ccount buffer = do
    let dataReg  = dataRegion buffer
    dynDataReg  <- dynDataRegion buffer
    Memory.withUnmanagedPtr (unwrap dynDataReg) $ \ptr
        -> flip (State.evalT @StoreDynState) (StoreDynState ptr)
         $ copyInitializeComponents__ @(Graph.ComponentsM m) ccount dataReg
{-# INLINE copyInitializeComponents #-}



-- === ComponentInitializerFold === --

type ComponentInitializerFold m comp
   = Fold.Builder1 (Fold.Scoped CopyInitialization) m (Component comp)

foldInitializeComponent :: ComponentInitializerFold m comp => Component comp layout -> m ()
foldInitializeComponent = \comp -> Fold.build1 @(Fold.Scoped CopyInitialization) comp (pure ())

class ComponentStaticInitializer__ (comps :: [Type]) m where
    copyInitializeComponents__ :: [Int] -> BufferDataRegion -> m ()

instance Applicative m
      => ComponentStaticInitializer__ '[] m where
    copyInitializeComponents__ = \_ _ -> pure ()
    {-# INLINE copyInitializeComponents__ #-}

instance
    ( layers ~ Graph.ComponentLayersM m comp
    , Layer.KnownByteSize layers
    , ComponentInitializerFold m comp
    , ComponentStaticInitializer__ comps m
    , MonadIO m
    -- debug:
    , Type.Show comp
    )
      => ComponentStaticInitializer__ (comp ': comps) m where
    copyInitializeComponents__ = \(compCount : compCounts) region -> do
        let compSize   = Layer.byteSize @layers
            regionPtr  = unwrap region
            regionPtr' = wrap $ regionPtr `Memory.plus` (compSize * compCount)
        putStrLn $ "copyInitializeComponents for " <> Type.show @comp <> " (" <> show compCount <> ")"
        flip mapM_ [0 .. compCount - 1] $ \ix -> do
            putStrLn $ ">> " <> show ix <> " (" <> show region <> ")"
            let ptr  = regionPtr `Memory.plus` (ix * compSize)
            Memory.withUnmanagedPtr ptr $ \uptr ->
                let comp = Component.unsafeFromPtr @comp (unwrap uptr)
                in  foldInitializeComponent comp

        copyInitializeComponents__ @comps compCounts regionPtr'



---------------------------------
-- === Pointer redirection === --
---------------------------------


type ComponentStaticRedirection comps m
    = ( ComponentStaticRedirection__ comps (State.StateT RedirectMap m)
      , Monad m
      )
redirectComponents :: ∀ comps m. ComponentStaticRedirection comps m
    => RedirectMap -> [Int] -> BufferDataRegion -> m ()
redirectComponents = \m ccount region -> flip (State.evalT @RedirectMap) m
    $ redirectComponents__ @comps ccount region


class ComponentStaticRedirection__ (comps :: [Type]) m where
    redirectComponents__ :: [Int] -> BufferDataRegion -> m ()

instance Applicative m
      => ComponentStaticRedirection__ '[] m where
    redirectComponents__ = \_ _ -> pure ()
    {-# INLINE redirectComponents__ #-}

instance
    ( layers ~ Graph.ComponentLayersM m comp
    , Layer.KnownByteSize layers
    , ComponentRedirectFold m comp
    , ComponentStaticRedirection__ comps m
    , MonadIO m
    -- debug:
    , Type.Show comp
    )
      => ComponentStaticRedirection__ (comp ': comps) m where
    redirectComponents__ = \(compCount : compCounts) region -> do
        let compSize   = Layer.byteSize @layers
            regionPtr  = unwrap region
            regionPtr' = wrap $ regionPtr `Memory.plus` (compSize * compCount)
        putStrLn $ "redirectComponents for " <> Type.show @comp <> " (" <> show compCount <> ")"
        flip mapM_ [0 .. compCount - 1] $ \ix -> do
            putStrLn $ ">> " <> show ix <> " (" <> show region <> ")"
            let ptr  = regionPtr `Memory.plus` (ix * compSize)
            Memory.withUnmanagedPtr ptr $ \uptr ->
                let comp = Component.unsafeFromPtr @comp (unwrap uptr)
                in  foldRedirectComponent comp

        redirectComponents__ @comps compCounts regionPtr'

data ComponentRedirection
type instance Fold.Result     ComponentRedirection = ()
type instance Fold.LayerScope ComponentRedirection = 'Fold.All

instance Monad m => Fold.ComponentMap ComponentRedirection m comp

instance (MonadIO m, PointerRedirection1 m (Layer.Cons layer), State.Getter RedirectMap m)
      => Fold.LayerMap ComponentRedirection m layer where
        mapLayer = \a _ -> do
            m <- State.get @RedirectMap
            let f ptr = do
                    print $ "LOOKUP " <> show ptr
                    pure . unsafeFromJust . flip Map.lookup m $ ptr
            (,()) <$> redirectPointers1 f a

            -- Fold.build1 @ComponentRedirection



type ComponentRedirectFold m comp
   = Fold.Builder1 (Fold.ScopedMap ComponentRedirection) m (Component comp)

foldRedirectComponent :: ComponentRedirectFold m comp => Component comp layout -> m ()
foldRedirectComponent = \comp -> Fold.build1 @(Fold.ScopedMap ComponentRedirection) comp (pure ())

class PointerRedirection1 m a where
    redirectPointers1 :: (Memory.SomeUnmanagedPtr -> m Memory.SomeUnmanagedPtr)
                      -> a t1 -> m (a t1)

class PointerRedirection m a where
    redirectPointers :: (Memory.SomeUnmanagedPtr -> m Memory.SomeUnmanagedPtr)
                     -> a -> m a

    default redirectPointers :: Applicative m => (Memory.SomeUnmanagedPtr -> m Memory.SomeUnmanagedPtr) -> a -> m a
    redirectPointers = \_ -> pure

instance MonadIO m
      => PointerRedirection1 m (ComponentSetA alloc tag) where
    redirectPointers1 = \f a -> do
        print ">> redirection ComponentSetA"
        out <- a <$ Mutable.mapM a (redirectPointers1 f)
        print ">> redirection ComponentSetA"
        pure out
    {-# INLINE redirectPointers1 #-}

instance Applicative m => PointerRedirection1 m (Component comp) where
    redirectPointers1 = \f a -> Component.unsafeFromPtr
        . unwrap <$> f (wrap $ Component.unsafeToPtr a)


instance MonadIO m => PointerRedirection m (ComponentVectorA alloc tag layout) where
    redirectPointers = \f a -> do
        print ">> redirection ComponentVectorA"
        out <- a <$ Mutable.mapM a (redirectPointers1 f)
        print "<< redirection ComponentVectorA"
        pure out
    {-# INLINE redirectPointers #-}

instance Applicative m => PointerRedirection m (Component comp layout) where
    redirectPointers = redirectPointers1

instance Applicative m => PointerRedirection m Word8
instance Applicative m => PointerRedirection m Word16
instance Applicative m => PointerRedirection m Word32
instance Applicative m => PointerRedirection m Word64
instance Applicative m => PointerRedirection m Bool

-- instance MonadIO m
--       => PointerRedirection m (ComponentSetA alloc tag layout) where
--     redirectPointers = \a f -> Mutable.mapM a $ \comp
--         -> pure $ Component.unsafeFromPtr $ unwrap $ f (wrap $ Component.unsafeToPtr comp)
--     {-# INLINE redirectPointers #-}

-- instance MonadIO m
--       => Fold.Builder1 ComponentRedirection m (ComponentSetA alloc tag) where
--     build1 = \a x -> x <* redirectPointers a undefined



--------------------------
-- === MemoryRegion === --
--------------------------

-- === Definition === --

newtype Buffer graph = Buffer (ManagedStruct (BufferLayout graph))

type HeaderLayout graph =
   '[ "staticDataRegionSize"  -:: Int
    , "dynamicDataRegionSize" -:: Int
    , "pointerDataRegionSize" -:: Int
    , "componentElems"        -:: ManagedArray (Graph.ComponentNumber graph) Int
    ]

type BufferDataRegion = UnknownSizeMemRegion 'Memory.Managed ()

type BufferLayout graph =
    HeaderLayout graph <>
   '[ "memoryRegion" -:: BufferDataRegion
    ]

instance Struct.IsStruct (Buffer graph)
type instance Memory.Management (Buffer graph) = 'Memory.Managed


-- === Aliases === --

type BufferM       m = Buffer       (Graph.Discover m)
type HeaderLayoutM m = HeaderLayout (Graph.Discover m)


-- === Fields === --

field_staticDataRegionSize  :: Struct.FieldRef "staticDataRegionSize"
field_dynamicDataRegionSize :: Struct.FieldRef "dynamicDataRegionSize"
field_pointerDataRegionSize :: Struct.FieldRef "pointerDataRegionSize"
field_componentElems        :: Struct.FieldRef "componentElems"
field_memoryRegion          :: Struct.FieldRef "memoryRegion"
field_staticDataRegionSize  = Struct.field ; {-# INLINE field_staticDataRegionSize  #-}
field_dynamicDataRegionSize = Struct.field ; {-# INLINE field_dynamicDataRegionSize #-}
field_pointerDataRegionSize = Struct.field ; {-# INLINE field_pointerDataRegionSize #-}
field_componentElems        = Struct.field ; {-# INLINE field_componentElems        #-}
field_memoryRegion          = Struct.field ; {-# INLINE field_memoryRegion          #-}


dataRegion :: Graph.KnownComponentNumber graph
    => Buffer graph -> BufferDataRegion
dataRegion = coerce . Struct.fieldPtr field_memoryRegion
{-# INLINE dataRegion #-}

dynDataRegion :: (Graph.KnownComponentNumber graph, MonadIO m)
    => Buffer graph -> m BufferDataRegion
dynDataRegion = \a -> do
    staticSize <- Struct.readField field_staticDataRegionSize  a
    pure $ coerce (Struct.fieldPtr field_memoryRegion a `Memory.plus` staticSize)
{-# INLINE dynDataRegion #-}

componentElems :: Buffer graph -> ManagedArray (Graph.ComponentNumber graph) Int
componentElems = coerce . Struct.fieldPtr field_componentElems
{-# INLINE componentElems #-}


-- === API === --

type Alloc m = (MonadIO m, Graph.KnownComponentNumberM m)

alloc :: ∀ m. Alloc m => [Int] -> Size -> m (BufferM m)
alloc = \ccount size -> liftIO $ do
    let headerSize = Storable.constantSize @(HeaderLayoutM m)
        bodySize   = Size.total size
        totalSize  = headerSize + bodySize
        staticRegionSize = size ^. Size.static
        dataRegionSize   = size ^. (Size.dynamic . Size.dataRegion)
        ptrRegionSize    = size ^. (Size.dynamic . Size.ptrRegion)

    ptr <- Memory.mallocBytes totalSize

    let struct   = Struct.unsafeCastFromPtr ptr
        elsCount = componentElems struct

    Struct.writeField field_staticDataRegionSize  struct staticRegionSize
    Struct.writeField field_dynamicDataRegionSize struct dataRegionSize
    Struct.writeField field_pointerDataRegionSize struct ptrRegionSize
    Mutable.unsafeWriteFromList elsCount ccount

    pure struct


-- === Conversions === --

unsafeFreeze :: (MonadIO m, Graph.KnownComponentNumber graph)
    => Buffer graph -> m ByteString
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


-- === Instances === --

makeLenses ''Buffer
deriving instance Show (Buffer graph)



---------------------------------
-- === StaticRegionEncoder === --
---------------------------------

-- === Definition === --


type StaticRegionEncoder comps m
    = (StaticRegionEncoder__ comps (State.StateT RedirectMap m), Monad m)

encodeStaticRegion :: StaticRegionEncoder comps m
    => Partition.Clusters comps -> Memory.SomeUnmanagedPtr -> m RedirectMap
encodeStaticRegion = flip State.execT mempty .: encodeStaticRegion__


-- === Internal === --

class StaticRegionEncoder__ comps m where
    encodeStaticRegion__ :: Partition.Clusters comps
                         -> Memory.SomeUnmanagedPtr
                         -> m Memory.SomeUnmanagedPtr

instance Applicative m
      => StaticRegionEncoder__ '[] m where
    encodeStaticRegion__ = \_ -> pure
    {-# INLINE encodeStaticRegion__ #-}

instance
    ( Partition.SplitHead comp comps
    , StaticRegionEncoder__ comps m
    , StaticComponentEncoder__ comp m
    , Monad m
    ) => StaticRegionEncoder__ (comp ': comps) m where
    encodeStaticRegion__ = \clusters ptr -> do
        let (!compList, !clusters') = Partition.splitHead clusters
        ptr' <- ComponentList.foldlM encodeComponentStatic__ ptr compList
        encodeStaticRegion__ clusters' ptr'
    {-# INLINE encodeStaticRegion__ #-}

class StaticComponentEncoder__ comp m where
    encodeComponentStatic__
        :: Memory.SomeUnmanagedPtr
        -> Component.Some comp
        -> m Memory.SomeUnmanagedPtr

instance
    ( layers ~ Graph.ComponentLayersM m comp
    , Layer.KnownByteSize layers
    , State.Monad RedirectMap m
    , MonadIO m
    ) => StaticComponentEncoder__ comp m where
    encodeComponentStatic__ = \tgtPtr comp -> do
        let srcPtr   = Convert.convert (Component.unsafeToPtr comp)
            compSize = Layer.byteSize @layers
        putStrLn $ "copyAndOffsetBytes " <> show (tgtPtr, srcPtr, compSize)
        nextSrcPtr <- Memory.copyAndOffsetBytes tgtPtr srcPtr compSize
        State.modify_ @RedirectMap $ Map.insert srcPtr tgtPtr
        return nextSrcPtr
    {-# INLINE encodeComponentStatic__ #-}










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
