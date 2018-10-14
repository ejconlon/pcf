module Pcf.Functions where

import Bound (Scope, instantiate1)
import Control.Monad (guard, mzero)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Gen (gen)
import Pcf.Types

assertTy :: Ord a => Ty -> Map a Ty -> Exp a -> TyM a ()
assertTy t env e = (== t) <$> typeCheck env e >>= guard

instantiateAndThen :: Ord a => Ty -> Map a Ty -> Scope () Exp a -> (Map a Ty -> Exp a -> TyM a b) -> TyM a b
instantiateAndThen ty env bind f = do
    v <- gen
    let env' = Map.insert v ty env
        body = instantiate1 (Var v) bind
    f env' body

typeCheck :: Ord a => Map a Ty -> Exp a -> TyM a Ty
typeCheck env (Var a) = maybe mzero pure (Map.lookup a env)
typeCheck env (App f a) = do
    fTy <- typeCheck env f
    case fTy of
        Arr aTy bTy -> assertTy aTy env a *> pure bTy
        _ -> mzero
typeCheck env (Ifz i t e) = do
    assertTy Nat env i
    tTy <- typeCheck env t
    instantiateAndThen Nat env e (assertTy tTy)
    pure tTy
typeCheck env (Lam aTy bind) = do
    bTy <- instantiateAndThen aTy env bind typeCheck
    pure (Arr aTy bTy)
typeCheck env (Fix ty bind) = do
    instantiateAndThen ty env bind (assertTy ty)
    pure ty
typeCheck env (Suc e) = assertTy Nat env e *> pure Nat
typeCheck _ Zero = pure Nat
