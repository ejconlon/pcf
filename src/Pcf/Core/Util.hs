{-# LANGUAGE Rank2Types #-}

module Pcf.Core.Util where

import           Control.Lens         (Lens', assign, over, use)
import           Control.Monad        (join, void)
import           Control.Monad.Reader (MonadReader, local)
import           Control.Monad.State  (MonadState)
import           Data.Map             (Map)
import qualified Data.Map             as M
import           Data.Sequence        (Seq (..))
import qualified Data.Sequence        as Seq

trabind :: (Traversable f, Monad f, Applicative m) => f a -> (a -> m (f b)) -> m (f b)
trabind fa k = join <$> traverse k fa

izipWithM_ :: Applicative m => (Int -> a -> b -> m ()) -> Seq a -> Seq b -> m ()
izipWithM_ f = go 0 where
    go i (a :<| as') (b :<| bs') = f i a b *> go (i + 1) as' bs'
    go _ _ _                     = pure ()

localMod :: MonadReader x m => Lens' x y -> (y -> y) -> m z -> m z
localMod lens mod = local (over lens mod)

modifyingM :: MonadState x m => Lens' x y -> (y -> m y) -> m y
modifyingM l f = do
    v <- use l
    v' <- f v
    assign l v'
    pure v

modifyingM_ :: MonadState x m => Lens' x y -> (y -> m y) -> m ()
modifyingM_ l f = void (modifyingM l f)

insertAll :: (Foldable t, Ord a) => t (a, b) -> Map a b -> Map a b
insertAll nxs m0 = foldl (\m (n, x) -> M.insert n x m) m0 nxs

filterMap :: (a -> Maybe b) -> Seq a -> Seq b
filterMap f s =
    case s of
        x :<| xs ->
            let xs' = filterMap f xs
            in case f x of
                Nothing -> xs'
                Just x' -> x' :<| xs'
        Empty -> Empty

zipWithIndex :: Seq a -> Seq b -> Seq (Int, a, b)
zipWithIndex = go 0 where
    go i (a :<| as) (b :<| bs) = (i, a, b) :<| go (i+1) as bs
    go _ _ _                   = Empty

findL :: (a -> Maybe b) -> Seq a -> Maybe b
findL f s =
    case s of
        x :<| xs ->
            let y = f x
            in case f x of
                Nothing -> findL f xs
                _       -> y

lookupR :: Eq a => a -> Seq (a, b) -> Maybe b
lookupR a abs = do
    i <- Seq.findIndexR (\(x, _) -> x == a) abs
    ab <- Seq.lookup i abs
    pure (snd ab)

ensure :: Monad m => (a -> m ()) -> a -> m a
ensure f a = f a >> pure a
