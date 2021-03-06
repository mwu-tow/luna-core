module Text.Parser.Combinators where

-- import qualified Text.Megaparsec as Parsec

import Control.Applicative as Applicative
import Control.Lens
import Control.Monad
import Data.List.NonEmpty
import Prelude
-- import Text.Megaparsec     (MonadParsec)


-- infix 1 <?>
-- (<?>) :: MonadParsec e s m => m a -> String -> m a
-- (<?>) = (Parsec.<?>) ; {-# INLINE (<?>) #-}


a <**?> f = a <**> option id f
f <?*>  a = option id f <*> a
f ?$    a = f a <|> pure a


option :: Alternative m => a -> m a -> m a
option x p = p <|> pure x

option_ :: Alternative m => m a -> m ()
option_ = option () . void

optionMaybe :: Alternative m => m a -> m (Maybe a)
optionMaybe = option Nothing . fmap Just

optionSkip :: Alternative m => a -> m b -> m a
optionSkip s p = option s (s <$ p)

boolOption :: Alternative m => m a -> m Bool
boolOption p = option False (True <$ p)


-- | Evaluates action `p` and passes its reult to function f.
--   If `p` failed, default result `def` is returned.
--   However if `p` succeeded and `f` failed, the whole computation fails as well.
tryBind :: (Monad m, Alternative m) => b -> m a -> (a -> m b) -> m b
tryBind def p f = join $ ($ f) <$> option (const $ pure def) ((&) <$> p)


some :: (Applicative m, Alternative m) => m a -> m (NonEmpty a)
some p = (:|) <$> p <*> many p ; {-# INLINE some #-}

someAsList :: (Applicative m, Alternative m) => m a -> m [a]
someAsList = Applicative.some ; {-# INLINE someAsList #-}



