module Pcf.Functions where

import           Bound                      (Scope (..), Var (B), abstract, abstract1,
                                             instantiate, instantiate1, (>>>=))
import           Bound.Name                 (Name (..))
import           Control.Applicative        (Alternative (..))
import           Control.Lens               (assign, use)
import           Control.Monad              (guard)
import           Control.Monad.State.Strict (MonadState (..), State, StateT)
import           Control.Monad.Trans.Maybe  (MaybeT (..))
import           Data.Foldable              (toList)
import           Data.Functor               (($>))
import           Data.Functor.Identity      (Identity (..))
import           Data.List                  (foldl')
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import           Data.Text                  (Text)
import           Data.Traversable           (for)
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
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

boundN :: Applicative f => Int -> Scope Int f a
boundN = Scope . pure . B

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

assertTyRaw :: (Monad m, Alternative m, Ord a) => (Text -> m a) -> Ty -> Map a Ty -> Exp a -> m ()
assertTyRaw gen t env e = (== t) <$> typeCheckRaw gen env e >>= guard

assertTy :: Ty -> Map Text Ty -> Exp Text -> Maybe ()
assertTy t env = runIdentity . runMaybeT . assertTyRaw pure t env

typeCheckRaw :: (Monad m, Alternative m, Ord a) => (Text -> m a) -> Map a Ty -> Exp a -> m Ty
typeCheckRaw _ env (Var a) = maybe empty pure (M.lookup a env)
typeCheckRaw gen env (App f x) = do
    fTy <- typeCheckRaw gen env f
    case fTy of
        Arr aTy bTy -> assertTyRaw gen aTy env x $> bTy
        _           -> empty
typeCheckRaw gen env (Ifz g t e) = do
    assertTyRaw gen Nat env g
    tTy <- typeCheckRaw gen env t
    assertTyRaw gen (Arr Nat tTy) env e
    pure tTy
typeCheckRaw gen env (Lam (Name n _) aTy bind) = do
    a <- gen n
    bTy <- instantiateAndThen a aTy env bind (typeCheckRaw gen)
    pure (Arr aTy bTy)
typeCheckRaw gen env (Fix (Name n _) ty bind) = do
    a <- gen n
    instantiateAndThen a ty env bind (assertTyRaw gen ty)
    pure ty
typeCheckRaw gen env (Suc e) = assertTyRaw gen Nat env e $> Nat
typeCheckRaw _ _ Zero = pure Nat

typeCheck :: Map Text Ty -> Exp Text -> Maybe Ty
typeCheck env = runIdentity . runMaybeT . typeCheckRaw pure env

typeCheckTop :: Exp Text -> Maybe Ty
typeCheckTop = typeCheck M.empty

bigStepRaw :: (Monad m, Alternative m, Ord a) => (Text -> m a) -> Map a (Exp a) -> Exp a -> m (Exp a)
bigStepRaw gen env (Var a) = maybe empty (bigStepRaw gen env) (M.lookup a env)
bigStepRaw gen env (App f x) = do
    fv <- bigStepRaw gen env f
    case fv of
        Lam (Name n _) _ bind -> do
            a <- gen n
            xv <- bigStepRaw gen env x
            instantiateAndThen a xv env bind (bigStepRaw gen)
        -- TODO (App (Fix ...) ...)
        _ -> empty
bigStepRaw gen env (Ifz g t e) = do
    iv <- bigStepRaw gen env g
    case iv of
        Zero -> bigStepRaw gen env t
        Suc ev ->
            case e of
                Lam (Name n _) _ bind -> do
                    a <- gen n
                    instantiateAndThen a ev env bind (bigStepRaw gen)
                -- TODO Allow Fix
                _ -> empty
        _ -> empty
bigStepRaw gen env v@Lam{} = pure v
bigStepRaw gen env v@Fix{} = pure v
bigStepRaw gen env v@Zero = pure v
bigStepRaw gen env (Suc e) = Suc <$> bigStepRaw gen env e

bigStep :: Map Text (Exp Text) -> Exp Text -> Maybe (Exp Text)
bigStep env = runIdentity . runMaybeT . bigStepRaw pure env

bigStepTop :: Exp Text -> Maybe (Exp Text)
bigStepTop = bigStep M.empty

closConvRaw :: (Monad m, Eq a) => (Text -> m a) -> Exp a -> m (ExpC a)
closConvRaw _ (Var a) = pure (VarC a)
closConvRaw gen (App l r) = AppC <$> closConvRaw gen l <*> closConvRaw gen r
closConvRaw gen (Ifz g t e) = IfzC <$> closConvRaw gen g <*> closConvRaw gen t <*> closConvRaw gen e
closConvRaw gen (Suc e) = SucC <$> closConvRaw gen e
closConvRaw _ Zero = pure ZeroC
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

varOnlyC :: (Alternative m, Traversable t) => t (ExpC a) -> m (t a)
varOnlyC = traverse $ \case
    VarC a -> pure a
    _      -> empty

nameL :: BindL a -> Text
nameL (RecL (Name n _) _ _ _) = n
nameL (NRecL (Name n _) _ _ _) = n

lambdaLiftRaw :: (Monad m, Alternative m, Eq a) => (Text -> m a) -> ExpC a -> m (ExpL a)
lambdaLiftRaw _ (VarC a) = pure (VarL a)
lambdaLiftRaw gen (AppC l r) = AppL <$> lambdaLiftRaw gen l <*> lambdaLiftRaw gen r
lambdaLiftRaw gen (IfzC g t e) = IfzL <$> lambdaLiftRaw gen g <*> lambdaLiftRaw gen t <*> lambdaLiftRaw gen e
lambdaLiftRaw gen (SucC e) = SucL <$> lambdaLiftRaw gen e
lambdaLiftRaw _ ZeroC = pure ZeroL
lambdaLiftRaw gen (LamC i@(Name n _) ty c b) = do
    a <- gen n
    c' <- varOnlyC c
    b' <- scopeRebindLam a c' b (lambdaLiftRaw gen)
    let bind = NRecL i ty (VarL <$> c') b'
    pure (LetL (V.singleton bind) (boundN 0))
lambdaLiftRaw gen (FixC i@(Name n _) ty c b) = do
    a <- gen n
    c' <- varOnlyC c
    b' <- scopeRebindLam a c' b (lambdaLiftRaw gen)
    let bind = RecL i ty (VarL <$> c') b'
    pure (LetL (V.singleton bind) (boundN 0))

lambdaLift :: ExpC Text -> Maybe (ExpL Text)
lambdaLift = runIdentity . runMaybeT . lambdaLiftRaw pure

data FauxState = FauxState deriving (Eq, Show)

emptyFauxState :: FauxState
emptyFauxState = FauxState

fauxBindLift :: MonadState FauxState m => (FunId -> ClosFC a -> BindFC a) -> ClosL a -> Scope Int ExpL a -> m (BindFC a)
fauxBindLift mk c b = undefined

fauxBind :: MonadState FauxState m => BindL a -> m (BindFC a)
fauxBind (NRecL i ty c b) = fauxBindLift (NRecFC i ty) c b
fauxBind (RecL i ty c b) = fauxBindLift (RecFC i ty) c b

-- TODO maybe don't need gen after all...
faux :: MonadState FauxState m => ExpL Text -> m (ExpFC Text)
faux (VarL a) = pure (VarFC a)
faux (AppL l r) = AppFC <$> faux l <*> faux r
faux (IfzL g t e) = IfzFC <$> faux g <*> faux t <*> faux e
faux (SucL e) = SucFC <$> faux e
faux ZeroL = pure ZeroFC
faux (LetL xs b) = do
    xs' <- traverse fauxBind xs
    let cs = nameL <$> xs
    b' <- scopeRebindLet cs b faux
    pure (LetFC xs' b')
