module Pcf.Core.UnionFind where

import Control.Monad.State (State, evalState, get, put)
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

newtype V = V { unV :: Int } deriving (Show, Eq, Ord, Enum)

data BiMap a = BiMap { fwdBM :: Map a V, bwdBM :: Map V (Set a), nextBM :: V } deriving (Show, Eq)

emptyBiMap :: BiMap a
emptyBiMap = BiMap M.empty M.empty (V 0)

lookupBM :: Ord a => a -> State (BiMap a) V
lookupBM a = do
    BiMap fwd bwd next <- get
    case M.lookup a fwd of
        Just v -> pure v
        Nothing -> do
            let bm' = BiMap (M.insert a next fwd) (M.insert next (S.singleton a) bwd) (succ next)
            put bm'
            pure next

reinsertBM :: Ord a => V -> V -> State (BiMap a) ()
reinsertBM fromV toV = do
    BiMap fwd bwd next <- get
    case M.lookup fromV bwd of
        Nothing -> pure ()
        Just as -> do
            let fwd' = S.foldl' (\m a -> M.insert a toV m) fwd as
                bm' = BiMap fwd' bwd next
            put bm'

linkBM :: Ord a => a -> a -> State (BiMap a) ()
linkBM a b = do
    av <- lookupBM a
    bv <- lookupBM b
    if av == bv
        then pure ()
        else if a > b
            then reinsertBM av bv
            else reinsertBM bv av

foldAct :: (Ord a, Ord x) => (Map a a, Map x a) -> a -> x -> (Map a a, Map x a)
foldAct (maa, mxa) a x =
    case M.lookup x mxa of
        Just b -> (M.insert a b maa, mxa)
        Nothing -> (maa, M.insert x a mxa)

projectBM :: Ord a => State (BiMap a) (Map a a)
projectBM = do
    BiMap fwd _ _ <- get
    let (maa, _) = M.foldlWithKey' foldAct (M.empty, M.empty) fwd
    pure maa

buildEqs :: (Foldable f, Ord a) => f (a, a) -> Map a a
buildEqs faa = evalState (traverse_ (uncurry linkBM) faa *> projectBM) emptyBiMap
