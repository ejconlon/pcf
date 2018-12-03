module Pcf.Functions where

import           Bound                 (Scope, abstract1, instantiate1)
import           Bound.Name            (Name (..))
import           Control.Monad         (guard, mzero)
import           Data.Functor          (($>))
import           Data.Functor.Identity (Identity)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import           Data.Text             (Text)
import           Pcf.Types

lam :: Eq a => Text -> a -> Ty -> Exp a -> Exp a
lam n a ty b = Lam (Name n ()) ty (abstract1 a b)

lam' :: Text -> Ty -> Exp Text -> Exp Text
lam' n = lam n n

fix :: Eq a => Text -> a -> Ty -> Exp a -> Exp a
fix n a ty b = Fix (Name n ()) ty (abstract1 a b)

fix' :: Text -> Ty -> Exp Text -> Exp Text
fix' n = fix n n

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
    in abstract1 v <$> f e

assertTy :: Ty -> Map Text Ty -> Exp Text -> Maybe ()
assertTy t env e = (== t) <$> typeCheck env e >>= guard

typeCheck :: Map Text Ty -> Exp Text -> Maybe Ty
typeCheck env (Var a) = maybe mzero pure (Map.lookup a env)
typeCheck env (App f a) = do
    fTy <- typeCheck env f
    case fTy of
        Arr aTy bTy -> assertTy aTy env a $> bTy
        _           -> mzero
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
typeCheck env (Suc e) = assertTy Nat env e $> Nat
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
                -- TODO Allow Fix
                _                     -> mzero
        _ -> mzero
bigStep env v@Lam{} = pure v
bigStep env v@Fix{} = pure v
bigStep env v@Zero = pure v
bigStep env (Suc e) = Suc <$> bigStep env e

bigStepTop :: Exp Text -> Maybe (Exp Text)
bigStepTop = bigStep Map.empty

-- newtype VarSet a = VarSet { unVarSet :: Map Text a } deriving (Show)

-- instance Eq Varset where
--     VarSet m == VarSet n = Map.keysSet m == Map.keysSet n

-- emptyVarSet :: VarSet a
-- emptyVarSet = VarSet Map.empty

-- freeVars :: VarSet a -> Exp a -> VarSet a
-- freeVars = runState

-- freeVarsTop :: Exp a -> VarSet a
-- freeVarsTop = freeVars emptyVarSet

-- closConv :: Eq a => Exp a -> Identity (ExpC a)
-- closConv (Var a) = pure (VarC a)
-- closConv (App l r) = AppC <$> closConv l <*> closConv r
-- closConv (Ifz g t e) = IfzC <$> closConv g <*> closConv t <*> closConv e
-- closConv (Suc e) = SucC <$> closConv e
-- closConv Zero = pure ZeroC
