
{-# LANGUAGE Rank2Types #-}

module Pcf.Core.BoundUtil where

import Bound (Scope(..), Var(..), abstract, abstract1, instantiate, instantiate1)
import Control.Monad.Reader (MonadReader, local)
import Control.Lens (Lens', over)
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Vector (Vector)
import qualified Data.Vector as V

-- Utils

localMod :: MonadReader x m => Lens' x y -> (y -> y) -> m z -> m z
localMod lens mod = local (over lens mod)

-- Scope manipulation

instantiateAndThen :: (MonadReader x m, Monad f, Ord a) => Lens' x (Map a w) -> a -> w -> Scope () f a -> (f a -> m r) -> m r
instantiateAndThen lens v w bind f = localMod lens (M.insert v w) (f (instantiate1 (pure v) bind))

instantiateAndThen' :: (Monad f, Eq a) => a -> Scope () f a -> (f a -> r) -> r
instantiateAndThen' v bind f =
    let body = instantiate1 (pure v) bind
    in f body

instantiateApply :: (Functor m, Monad f, Monad g, Eq a) => a -> Scope () f a -> (f a -> m (g a)) -> m (Scope () g a)
instantiateApply v bind f =
    let e = instantiate1 (pure v) bind
    in abstract1 v <$> f e

insertOnce :: Eq a => Vector a -> a -> Vector a
insertOnce vs a = if V.elem a vs then vs else V.snoc vs a

insertOnceWithout :: Eq a => a -> Vector a -> a -> Vector a
insertOnceWithout v vs a = if a == v then vs else insertOnce vs a

scopeRebind :: (Functor m, Monad f, Monad g, Foldable g, Eq a) => a -> Scope () f a -> (f a -> m (g a)) -> m (Vector a, Scope Int g a)
scopeRebind v bind f =
    let fbody = instantiate1 (pure v) bind
        mgbody = f fbody
        process gbody =
            let fvs = foldl' (insertOnceWithout v) V.empty (toList gbody)
                fvs' = V.snoc fvs v
                gbody' = abstract (`V.elemIndex` fvs') gbody
            in (fvs, gbody')
    in process <$> mgbody

scopeRebindLet :: (Functor m, Monad f, Monad g, Foldable g, Eq a) =>  Vector a -> Scope Int f a -> (f a -> m (g a)) -> m (Scope Int g a)
scopeRebindLet c' bind f =
    let fbody = instantiate (pure . (c' V.!)) bind
        mgbody = f fbody
    in abstract (`V.elemIndex` c') <$> mgbody

scopeRebindLam :: (Functor m, Monad f, Monad g, Foldable g, Eq a) => a -> Vector a -> Scope Int f a -> (f a -> m (g a)) -> m (Scope Int g a)
scopeRebindLam v c bind f =
    let c' = V.snoc c v
    in scopeRebindLet c' bind f

-- Hack!

boundN :: Applicative f => Int -> Scope Int f a
boundN = Scope . pure . B