module Luna.IR.Component.Link where

import Prologue
import Type.Data.Ord

import qualified OCI.IR.Component      as Component
import qualified OCI.IR.Layer.Internal as Layer
import qualified OCI.IR.Layout         as Layout

import Luna.IR.Component.Term.Class (Term)
import OCI.IR.Layout                ((:=), Layout)



------------------
-- === Link === --
------------------

-- === Definition === ---

Component.define "Link"
type SomeLink = Link ()
type src *-* tgt = Layout '[Source := src, Target := tgt]


-- === Construction === --

type Creator m =
    ( Component.Creator Links m
    , Layer.Writer Links Source m
    , Layer.Writer Links Target m
    )

new :: Creator m => Term src -> Term tgt -> m (Link (src *-* tgt))
new src tgt = do
    ir <- Component.new
    Layer.write @Source ir src
    Layer.write @Target ir tgt
    pure $ ir
{-# INLINE new #-}


-- === Instances === --

-- type instance Generalizable2 (Link l) (Link l') = Generalizable2 l l'
-- instance Generalizable l l' => Generalizable (Link l) (Link l')


--------------------
-- === Layers === --
--------------------

data Source
data Target

type instance Layer.Layout Links Source layout = Layout.Get Source layout
type instance Layer.Layout Links Target layout = Layout.Get Target layout
type instance Layer.Data   Links Source        = Term
type instance Layer.Data   Links Target        = Term

type instance Cmp Source Target = 'LT
type instance Cmp Target Source = 'GT



----------------------
-- === HasLinks === --
----------------------


class HasLinks a where
    readLinksIO :: a -> IO [SomeLink]

readLinks :: (HasLinks a, MonadIO m) => a -> m [SomeLink]
readLinks = liftIO . readLinksIO
{-# INLINE readLinks #-}

instance HasLinks (Link a) where

instance HasLinks Int where
    readLinksIO _ = pure mempty ; {-# INLINE readLinksIO #-}
