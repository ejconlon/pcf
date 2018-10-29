module Pcf.Functions where

import Bound (Scope, instantiate1)
import Control.Monad (guard, mzero)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Gen (gen)
import Pcf.Types

assertTy :: Ord a => Ty -> Map a Ty -> Exp a -> TyM a ()
assertTy t env e = (== t) <$> typeCheck env e >>= guard

instantiateAndThen :: Ord a => w -> Map a w -> Scope () Exp a -> (Map a w -> Exp a -> TyM a b) -> TyM a b
instantiateAndThen w env bind f = do
    v <- gen
    let env' = Map.insert v w env
        body = instantiate1 (Var v) bind
    f env' body

typeCheck :: Ord a => Map a Ty -> Exp a -> TyM a Ty
typeCheck env (Var a) = maybe mzero pure (Map.lookup a env)
typeCheck env (App f a) = do
    fTy <- typeCheck env f
    case fTy of
        Arr aTy bTy -> assertTy aTy env a *> pure bTy
        _ -> mzero
typeCheck env (Ifz i t bind) = do
    assertTy Nat env i
    tTy <- typeCheck env t
    instantiateAndThen Nat env bind (assertTy tTy)
    pure tTy
typeCheck env (Lam aTy bind) = do
    bTy <- instantiateAndThen aTy env bind typeCheck
    pure (Arr aTy bTy)
typeCheck env (Fix ty bind) = do
    instantiateAndThen ty env bind (assertTy ty)
    pure ty
typeCheck env (Suc e) = assertTy Nat env e *> pure Nat
typeCheck _ Zero = pure Nat

bigStep :: Ord a => Map a (Exp a) -> Exp a -> TyM a (Exp a)
bigStep env v@(Var a) = maybe mzero (bigStep env) (Map.lookup a env)
bigStep env (App f a) = do
    fv <- bigStep env f
    case fv of
        Lam _ bind -> do
            av <- bigStep env a
            instantiateAndThen av env bind bigStep
        _ -> mzero
bigStep env (Ifz i t bind) = do
    iv <- bigStep env i
    case iv of
        Zero -> bigStep env t
        Suc ev -> instantiateAndThen ev env bind bigStep
        _ -> mzero
bigStep env v@(Lam _ _) = pure v
bigStep env v@(Fix _ _) = pure v
bigStep env v@Zero = pure v
bigStep env (Suc e) = Suc <$> bigStep env e

closConv :: Exp a -> ExpC a
closConv Zero = ZeroC
