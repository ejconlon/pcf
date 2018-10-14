module Pcf.Functions where

import Bound (instantiate1)
import Control.Monad (guard, mzero)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Gen (gen)
import Pcf.Types

assertTy :: Ord a => Map a Ty -> Exp a -> Ty -> TyM a ()
assertTy env e t = (== t) <$> typeCheck env e >>= guard

typeCheck :: Ord a => Map a Ty -> Exp a -> TyM a Ty
typeCheck _ Zero = pure Nat
typeCheck env (Suc e) = assertTy env e Nat *> pure Nat
typeCheck env (Var a) = maybe mzero pure (Map.lookup a env)
typeCheck env (App f a) = do
    fTy <- typeCheck env f
    case fTy of
        Arr aTy bTy -> assertTy env a aTy *> pure bTy
        _ -> mzero
typeCheck env (Lam aTy bind) = do
    v <- gen
    let body = instantiate1 (Var v) bind
        env' = Map.insert v aTy env
    bTy <- typeCheck env' body
    pure (Arr aTy bTy)
