module Pcf.Functions where

import           Bound                     (Scope, abstract, abstract1, instantiate, instantiate1, (>>>=))
import           Bound.Name                (Name (..))
import           Control.Monad             (guard, mzero)
import           Control.Monad.Trans       (lift)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Foldable             (toList)
import           Data.Functor              (($>))
import           Data.Functor.Identity     (Identity (..))
import           Data.List                 (foldl')
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as M
import           Data.Set                  (Set)
import qualified Data.Set                  as S
import           Data.Text                 (Text)
import           Data.Vector               (Vector)
import qualified Data.Vector               as V
import           Pcf.Types

-- Scope manipulation

instantiateAndThen :: (Monad f, Ord a) => a -> w -> Map a w -> Scope () f a -> (Map a w -> f a -> r) -> r
instantiateAndThen v w env bind f =
    let env' = M.insert v w env
        body = instantiate1 (pure v) bind
    in f env' body

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

scopeRebind :: (Functor m, Monad f, Monad g, Foldable g, Eq a) => a -> Scope () f a -> (f a -> m (g a)) -> m (Vector a, Scope Int g a)
scopeRebind v bind f =
    let fbody = instantiate1 (pure v) bind
        mgbody = f fbody
        process gbody =
            let fvs = foldl' insertOnce V.empty (toList gbody)
                rebind a = if a == v then Just (V.length fvs) else V.elemIndex a fvs
                gbody' = abstract rebind gbody
            in (fvs, gbody')
    in process <$> mgbody

scopeRebindC :: (Monad m, Monad f, Monad g, Foldable g, Eq a) => a -> Vector a -> Scope Int f a -> (f a -> m (g a)) -> m (Scope Int g a)
scopeRebindC v c bind f = do
    let c' = V.snoc c v
        fbody = instantiate (pure . (c' V.!)) bind
    gbody <- f fbody
    pure (abstract (flip V.elemIndex c') gbody)

-- Smart constructors

lam :: Eq a => Text -> a -> Ty -> Exp a -> Exp a
lam n a ty b = Lam (Name n ()) ty (abstract1 a b)

lam' :: Text -> Ty -> Exp Text -> Exp Text
lam' n = lam n n

fix :: Eq a => Text -> a -> Ty -> Exp a -> Exp a
fix n a ty b = Fix (Name n ()) ty (abstract1 a b)

fix' :: Text -> Ty -> Exp Text -> Exp Text
fix' n = fix n n

-- Exp manipulation

freeVars :: Ord a => Exp a -> Set a
freeVars = S.fromList . toList

assertTyRaw :: (Monad m, Ord a) => (Text -> m a) -> Ty -> Map a Ty -> Exp a -> MaybeT m ()
assertTyRaw gen t env e = (== t) <$> typeCheckRaw gen env e >>= guard

assertTy :: Ty -> Map Text Ty -> Exp Text -> Maybe ()
assertTy t env = runIdentity . runMaybeT . assertTyRaw pure t env

typeCheckRaw :: (Monad m, Ord a) => (Text -> m a) -> Map a Ty -> Exp a -> MaybeT m Ty
typeCheckRaw _ env (Var a) = maybe mzero pure (M.lookup a env)
typeCheckRaw gen env (App f x) = do
    fTy <- typeCheckRaw gen env f
    case fTy of
        Arr aTy bTy -> assertTyRaw gen aTy env x $> bTy
        _           -> mzero
typeCheckRaw gen env (Ifz g t e) = do
    assertTyRaw gen Nat env g
    tTy <- typeCheckRaw gen env t
    assertTyRaw gen (Arr Nat tTy) env e
    pure tTy
typeCheckRaw gen env (Lam (Name n _) aTy bind) = do
    a <- lift (gen n)
    bTy <- instantiateAndThen a aTy env bind (typeCheckRaw gen)
    pure (Arr aTy bTy)
typeCheckRaw gen env (Fix (Name n _) ty bind) = do
    a <- lift (gen n)
    instantiateAndThen a ty env bind (assertTyRaw gen ty)
    pure ty
typeCheckRaw gen env (Suc e) = assertTyRaw gen Nat env e $> Nat
typeCheckRaw _ _ Zero = pure Nat

typeCheck :: Map Text Ty -> Exp Text -> Maybe Ty
typeCheck env = runIdentity . runMaybeT . typeCheckRaw pure env

typeCheckTop :: Exp Text -> Maybe Ty
typeCheckTop = typeCheck M.empty

bigStepRaw :: (Monad m, Ord a) => (Text -> m a) -> Map a (Exp a) -> Exp a -> MaybeT m (Exp a)
bigStepRaw gen env (Var a) = maybe mzero (bigStepRaw gen env) (M.lookup a env)
bigStepRaw gen env (App f x) = do
    fv <- bigStepRaw gen env f
    case fv of
        Lam (Name n _) _ bind -> do
            a <- lift (gen n)
            xv <- bigStepRaw gen env x
            instantiateAndThen a xv env bind (bigStepRaw gen)
        -- TODO (App (Fix ...) ...)
        _ -> mzero
bigStepRaw gen env (Ifz g t e) = do
    iv <- bigStepRaw gen env g
    case iv of
        Zero -> bigStepRaw gen env t
        Suc ev ->
            case e of
                Lam (Name n _) _ bind -> do
                    a <- lift (gen n)
                    instantiateAndThen a ev env bind (bigStepRaw gen)
                -- TODO Allow Fix
                _                     -> mzero
        _ -> mzero
bigStepRaw gen env v@Lam{} = pure v
bigStepRaw gen env v@Fix{} = pure v
bigStepRaw gen env v@Zero = pure v
bigStepRaw gen env (Suc e) = Suc <$> bigStepRaw gen env e

bigStep :: Map Text (Exp Text) -> Exp Text -> Maybe (Exp Text)
bigStep env = runIdentity . runMaybeT . bigStepRaw pure env

bigStepTop :: Exp Text -> Maybe (Exp Text)
bigStepTop = bigStep M.empty

closConvRaw :: (Monad m, Eq a) => (Text -> m a) -> Exp a -> m (ExpC a)
closConvRaw gen (Var a) = pure (VarC a)
closConvRaw gen (App l r) = AppC <$> closConvRaw gen l <*> closConvRaw gen r
closConvRaw gen (Ifz g t e) = IfzC <$> closConvRaw gen g <*> closConvRaw gen t <*> closConvRaw gen e
closConvRaw gen (Suc e) = SucC <$> closConvRaw gen e
closConvRaw gen Zero = pure ZeroC
closConvRaw gen (Lam i@(Name n _) ty b) = do
    a <- gen n
    (c, b') <- scopeRebind a b (closConvRaw gen)
    pure (LamC i ty (VarC <$> c) b')
closConvRaw gen (Fix i@(Name n _) ty b) = do
    a <- gen n
    (c, b') <- scopeRebind a b (closConvRaw gen)
    pure (FixC i ty (VarC <$> c) b')

closConv :: Exp Text -> ExpC Text
closConv = runIdentity . closConvRaw pure

lambdaLiftRaw :: (Monad m, Eq a) => (Text -> m a) -> ExpC a -> m (ExpL a)
lambdaLiftRaw = undefined -- TODO fill in

lambdaLift :: ExpC Text -> ExpL Text
lambdaLift = runIdentity . lambdaLiftRaw pure
