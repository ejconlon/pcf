module Pcf.Core.BoundCrazy where

import Bound         (Scope (..), Var (..))
import Control.Monad (join)

trabind :: (Traversable f, Monad f, Applicative m)
        => f a -> (a -> m (f b)) -> m (f b)
trabind fa k = join <$> traverse k fa

-- class Monad f => Munge f where
--     munge :: Applicative m => f a -> (a -> m (f b)) -> m (f b)

-- mungeBind :: Munge f => f a -> (a -> f b) -> f b
-- mungeBind m f = runIdentity (munge m (Identity . f))

-- mungeScope :: (Applicative m, Munge f) => Scope b f a -> (a -> m (f c)) -> m (Scope b f c)
-- mungeScope (Scope e) f = Scope <$> munge e g where
--     g v = case v of
--         B b  -> pure (pure (B b))
--         F ea -> munge ea (fmap (pure . F) . f)

instantiateM :: (Applicative m, Traversable f, Monad f) => (b -> m (f a)) -> Scope b f a -> m (f a)
instantiateM k (Scope e) = trabind e g where
    g v = case v of
        B b  -> k b
        F ea -> pure ea

instantiate1M :: (Applicative m, Traversable f, Monad f) => m (f a) -> Scope () f a -> m (f a)
instantiate1M = instantiateM . const

abstractM :: (Applicative m, Traversable f, Applicative f) => (a -> m (Maybe b)) -> f a -> m (Scope b f a)
abstractM f e = Scope <$> traverse g e where
    g a = h a <$> f a
    h a = maybe (F (pure a)) B

abstract1M :: (Applicative m, Traversable f, Applicative f, Eq a) => m a -> f a -> m (Scope () f a)
abstract1M ma = abstractM (\a1 -> (\a0 -> if a1 == a0 then Just () else Nothing) <$> ma)
