module Pcf.Functions where

import Bound (Scope, abstract1, instantiate1)
import Bound.Name (name)
import Control.Monad (guard, mzero)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Control.Monad.Gen (MonadGen(..))
import Pcf.Types

instantiateAndThen :: (MonadGen a m, Monad f, Ord a) => w -> Map a w -> Scope () f a -> (Map a w -> f a -> m b) -> m b
instantiateAndThen w env bind f = do
    v <- gen
    let env' = Map.insert v w env
        body = instantiate1 (pure v) bind
    f env' body

instantiateApply :: (MonadGen a m, Monad f, Monad g, Eq a) => Scope () f a -> (f a -> m (g a)) -> m (Scope () g a)
instantiateApply bind f = do
    v <- gen
    let fa = instantiate1 (pure v) bind
    ga <- f fa
    pure (abstract1 v ga)

assertTy :: Ord a => Ty -> Map a Ty -> Exp a -> TyM a ()
assertTy t env e = (== t) <$> typeCheck env e >>= guard

typeCheck :: Ord a => Map a Ty -> Exp a -> TyM a Ty
typeCheck env (Var a) = maybe mzero pure (Map.lookup a env)
typeCheck env (App f a) = do
    fTy <- typeCheck env f
    case fTy of
        Arr aTy bTy -> assertTy aTy env a *> pure bTy
        _ -> mzero
typeCheck env (Ifz g t e) = do
    assertTy Nat env g
    tTy <- typeCheck env t
    assertTy (Arr Nat tTy) env e
    pure tTy
typeCheck env (Lam _ aTy bind) = do
    bTy <- instantiateAndThen aTy env bind typeCheck
    pure (Arr aTy bTy)
typeCheck env (Fix _ ty bind) = do
    instantiateAndThen ty env bind (assertTy ty)
    pure ty
typeCheck env (Suc e) = assertTy Nat env e *> pure Nat
typeCheck _ Zero = pure Nat

bigStep :: Ord a => Map a (Exp a) -> Exp a -> TyM a (Exp a)
bigStep env v@(Var a) = maybe mzero (bigStep env) (Map.lookup a env)
bigStep env (App f a) = do
    fv <- bigStep env f
    case fv of
        Lam _ _ bind -> do
            av <- bigStep env a
            instantiateAndThen av env bind bigStep
        _ -> mzero
bigStep env (Ifz i t e) = do
    iv <- bigStep env i
    case iv of
        Zero -> bigStep env t
        Suc ev ->
            case e of
                Lam _ _ bind -> instantiateAndThen ev env bind bigStep
                _ -> mzero
        _ -> mzero
bigStep env v@(Lam _ _ _) = pure v
bigStep env v@(Fix _ _ _) = pure v
bigStep env v@Zero = pure v
bigStep env (Suc e) = Suc <$> bigStep env e

-- closConv :: Eq a => Exp a -> Gen a (ExpC a)
-- closConv (Var a) = pure (VarC a)
-- closConv (App l r) = AppC <$> closConv l <*> closConv r
-- closConv (Ifz g t n e) = do
--     let e' = instantiateApply n e closConv
--     IfzC <$> closConv g <*> closConv t <*> e'
-- closConv (Suc e) = SucC <$> closConv e
-- closConv Zero = pure ZeroC
