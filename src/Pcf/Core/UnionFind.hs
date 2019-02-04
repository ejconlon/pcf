module Pcf.Core.UnionFind (UnionFindC, UnionFindState, emptyUnionFindState, lookupUF, linkUF) where

import Control.Monad.Identity (Identity (..))
import Control.Monad.State (MonadState, State, evalState, get, put)
import Data.Foldable (traverse_)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

newtype V = V { unV :: Int } deriving (Show, Eq, Ord, Enum)

type UnionFindC a m = MonadState (UnionFindState a) m
type UnionFindT a b = State (UnionFindState a) b

ufProof :: (forall n. UnionFindC a n => n b) -> UnionFindT a b
ufProof = id

data UnionFindState a = UnionFindState
    { ufsFwd :: Map a V
    , ufsBwd :: Map V (a, Set a)
    , ufsNext :: V
    } deriving (Show, Eq)

emptyUnionFindState :: UnionFindState a
emptyUnionFindState = UnionFindState M.empty M.empty (V 0)

lookupUF :: (UnionFindC a m, Ord a) => a -> m a
lookupUF a = do
    UnionFindState fwd bwd _ <- get
    case M.lookup a fwd of
        Just v -> pure (fst (bwd M.! v))
        Nothing -> pure a

forwardUF :: (UnionFindC a m, Ord a) => a -> m V
forwardUF a = do
    UnionFindState fwd bwd next <- get
    case M.lookup a fwd of
        Just v -> pure v
        Nothing -> do
            put (UnionFindState (M.insert a next fwd) (M.insert next (a, S.singleton a) bwd) (succ next))
            pure next

reinsertUF :: (UnionFindC a m, Ord a) => a -> V -> V -> m ()
reinsertUF fromA fromV toV = do
    UnionFindState fwd bwd next <- get
    case M.lookup fromV bwd of
        Nothing -> pure ()
        Just (_, fromAs) -> do
            let fwd' = S.foldl' (\m a -> M.insert a toV m) fwd fromAs
                cleanBwd = M.delete fromV bwd
                (toA, toAs) = cleanBwd M.! toV
                toAs' = S.union toAs fromAs
                bwd' = M.insert toV (toA, toAs') cleanBwd
                bm' = UnionFindState fwd' bwd' next
            put bm'

linkUF :: (UnionFindC a m, Ord a) => a -> a -> m ()
linkUF a b = do
    av <- forwardUF a
    bv <- forwardUF b
    if av == bv
        then pure ()
        else if a > b
            then reinsertUF a av bv
            else reinsertUF b bv av

-- foldActUF :: Ord a => (Map a a, Map V a) -> (a, V) -> (Map a a, Map V a)
-- foldActUF (maa, mxa) (a, x) =
--     case M.lookup x mxa of
--         Just b -> (M.insert a b maa, mxa)
--         Nothing -> (maa, M.insert x a mxa)

-- projectUF :: (UnionFindC a m, Ord a) => m (Map a a)
-- projectUF = do
--     UnionFindState fwd _ _ <- get
--     let (maa, _) = foldl' foldActUF (M.empty, M.empty) (M.toList fwd)
--     pure maa

-- buildEqs :: (Foldable f, Ord a) => f (a, a) -> Map a a
-- buildEqs faa = evalState (traverse_ (uncurry linkUF) faa *> projectUF) emptyUnionFindState
