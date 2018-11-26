module Pcf.Functions where

import Bound (Scope, abstract1, instantiate1)
import Bound.Name (Name(..))
import Control.Monad (guard, mzero)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import Pcf.Types

instantiateAndThen :: (Monad f, Ord a) => a -> w -> Map a w -> Scope () f a -> (Map a w -> f a -> r) -> r
instantiateAndThen v w env bind f =
    let env' = Map.insert v w env
        body = instantiate1 (pure v) bind
    in f env' body

instantiateAndThen' :: (Monad f, Ord a) => a -> Scope () f a -> (f a -> r) -> r
instantiateAndThen' v bind f =
    let body = instantiate1 (pure v) bind
    in f body

instantiateApply :: (Functor m, Monad f, Monad g, Eq a) => a -> Scope () f a -> (f a -> m (g a)) -> m (Scope () g a)
instantiateApply v bind f =
    let e = instantiate1 (pure v) bind
    in (\e' -> abstract1 v e') <$> f e

assertTy :: Ty -> Map Text Ty -> Exp Text -> Maybe ()
assertTy t env e = (== t) <$> typeCheck env e >>= guard

typeCheck :: Map Text Ty -> Exp Text -> Maybe Ty
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
typeCheck env (Lam (Name n _) aTy bind) = do
    bTy <- instantiateAndThen n aTy env bind typeCheck
    pure (Arr aTy bTy)
typeCheck env (Fix (Name n _) ty bind) = do
    instantiateAndThen n ty env bind (assertTy ty)
    pure ty
typeCheck env (Suc e) = assertTy Nat env e *> pure Nat
typeCheck _ Zero = pure Nat

typeCheckTop :: Exp Text -> Maybe Ty
typeCheckTop = typeCheck Map.empty

bigStep :: Map Text (Exp Text) -> Exp Text -> Maybe (Exp Text)
bigStep env v@(Var a) = maybe mzero (bigStep env) (Map.lookup a env)
bigStep env (App f a) = do
    fv <- bigStep env f
    case fv of
        Lam (Name n _) _ bind -> do
            av <- bigStep env a
            instantiateAndThen n av env bind bigStep
        -- TODO (App (Fix ...) ...)
        _ -> mzero
bigStep env (Ifz i t e) = do
    iv <- bigStep env i
    case iv of
        Zero -> bigStep env t
        Suc ev ->
            case e of
                Lam (Name n _) _ bind -> instantiateAndThen n ev env bind bigStep
                -- TODO Allow Fix?
                _ -> mzero
        _ -> mzero
bigStep env v@(Lam _ _ _) = pure v
bigStep env v@(Fix _ _ _) = pure v
bigStep env v@Zero = pure v
bigStep env (Suc e) = Suc <$> bigStep env e

bigStepTop :: Exp Text -> Maybe (Exp Text)
bigStepTop = bigStep Map.empty

-- closConv :: Eq a => Exp a -> Gen a (ExpC a)
-- closConv (Var a) = pure (VarC a)
-- closConv (App l r) = AppC <$> closConv l <*> closConv r
-- closConv (Ifz g t n e) = do
--     let e' = instantiateApply n e closConv
--     IfzC <$> closConv g <*> closConv t <*> e'
-- closConv (Suc e) = SucC <$> closConv e
-- closConv Zero = pure ZeroC
