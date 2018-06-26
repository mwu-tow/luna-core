module Data.Mutable.Class where

import Prologue hiding (Read, unsafeRead)

import Foreign.Ptr (Ptr)



-- === Memory management === --

class New          m a where new          :: m a
class PlacementNew m a where placementNew :: Ptr a -> m a
class Alloc        m a where alloc        :: Int -> m a
class Free         m a where free         :: a -> m ()
class Grow         m a where grow         :: a -> m ()


-- === Properties === --

class Size         m a where size        :: a -> m Int
class Capacity     m a where capacity    :: a -> m Int


-- === Element management === --

class Read         m a where unsafeRead  :: a -> Int -> m (Item a)
class Write        m a where unsafeWrite :: a -> Int -> Item a -> m ()
class PushBack     m a where pushBack    :: a -> Item a -> m ()
class PushFront    m a where pushFront   :: a -> Item a -> m ()
class Insert       m a where insert      :: a -> Item a -> m ()
class Remove       m a where remove      :: a -> Item a -> m ()
class InsertAt     m a where insertAt    :: a -> Int -> Item a -> m ()
class RemoveAt     m a where removeAt    :: a -> Int -> m ()


-- === Conversions === --

class ToList       m a where toList      :: a -> m [Item a]
class FromList     m a where fromList    :: [Item a] -> m a





-- === Utils === --

unsafeWriteFromList :: (Write m a, Applicative m) => a -> [Item a] -> m ()
unsafeWriteFromList = \a -> zipWithM_ (unsafeWrite a) [0..]
{-# INLINE unsafeWriteFromList #-}


type Map m a = (Read m a, Write m a, Monad m)

unsafeMapM_ :: Map m a => a -> Int -> (Item a -> m (Item a)) -> m ()
unsafeMapM_ = \a ix f -> unsafeWrite a ix =<< f =<< unsafeRead a ix
{-# INLINE unsafeMapM_ #-}

unsafeMap_ :: Map m a => a -> Int -> (Item a -> Item a) -> m ()
unsafeMap_ = \a ix f -> unsafeMapM_ a ix (pure . f)
{-# INLINE unsafeMap_ #-}





