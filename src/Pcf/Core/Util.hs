{-# LANGUAGE Rank2Types #-}

module Pcf.Core.Util where

import Control.Lens (Lens', assign, over, use)
import Control.Monad (join)
import Control.Monad.Reader (MonadReader, local)
import Control.Monad.State (MonadState)
import           Data.Map (Map)
import qualified Data.Map as M
import Data.Sequence (Seq (..))

trabind :: (Traversable f, Monad f, Applicative m) => f a -> (a -> m (f b)) -> m (f b)
trabind fa k = join <$> traverse k fa

izipWithM_ :: Applicative m => (Int -> a -> b -> m ()) -> Seq a -> Seq b -> m ()
izipWithM_ f as bs = go 0 as bs where
    go i (a :<| as') (b :<| bs') = f i a b *> go (i + 1) as' bs'
    go _ _ _                     = pure ()

localMod :: MonadReader x m => Lens' x y -> (y -> y) -> m z -> m z
localMod lens mod = local (over lens mod)

modifyingM :: MonadState x m => Lens' x y -> (y -> m y) -> m ()
modifyingM l f = do
    v <- use l
    v' <- f v
    assign l v'

insertAll :: (Foldable t, Ord a) => t (a, b) -> Map a b -> Map a b
insertAll nts m0 = foldl (\m (n, t) -> M.insert n t m) m0 nts
