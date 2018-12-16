{-# LANGUAGE Rank2Types #-}

module Pcf.V1.Functions where

import           Bound                      (Scope (..), Var (B), abstract, abstract1,
                                             instantiate, instantiate1, (>>>=))
import           Bound.Name                 (Name (..))
import           Control.Applicative        (Alternative (..))
import           Control.Lens               (Lens', assign, over, use, view)
import           Control.Monad              (unless)
import           Control.Monad.Except       (MonadError (..))
import           Control.Monad.Reader       (MonadReader (..))
import           Control.Monad.State.Strict (MonadState (..), modify)
import           Control.Monad.Trans        (MonadTrans (..))
import           Data.Foldable              (toList)
import           Data.Functor               (($>))
import           Data.Generics.Product      (field)
import           Data.List                  (foldl')
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import           Data.Text                  (Text)
import           Data.Traversable           (for)
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           Data.Void                  (Void)
import           GHC.Generics               (Generic)
import           Pcf.Core.Func
import           Pcf.V1.Types

-- Utils

localMod :: MonadReader x m => (Lens' x y) -> (y -> y) -> m z -> m z
localMod lens mod = local (over lens mod)

-- Scope manipulation

instantiateAndThen :: (MonadReader x m, Monad f, Ord a) => (Lens' x (Map a w)) -> a -> w -> Scope () f a -> (f a -> m r) -> m r
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

-- Pure ops

freeVars :: Ord a => Exp a -> Set a
freeVars = S.fromList . toList

-- VarGen

type VarGen m a = Text -> m a

-- Typing

data TypeError a =
      CheckError Ty Ty
    | TypeMissingVarError a
    | AppNotArrError Ty
    deriving (Eq, Show)

data TypeEnv m a = TypeEnv
    { teGen   :: VarGen m a
    , teTyMap :: Map a Ty
    } deriving (Generic)

type TypeT a m = FuncT (TypeEnv m a) () (TypeError a) m

assertTy :: (Monad m, Ord a) => Ty -> Exp a -> TypeT a m ()
assertTy t e = do
    u <- typeCheck e
    unless (u == t) (throwError (CheckError u t))

typeCheck :: (Monad m, Ord a) => Exp a -> TypeT a m Ty
typeCheck (Var a) = do
    tyMap <- view (field @"teTyMap")
    maybe (throwError (TypeMissingVarError a)) pure (M.lookup a tyMap)
typeCheck (App f x) = do
    fTy <- typeCheck f
    case fTy of
        Arr aTy bTy -> assertTy aTy x $> bTy
        _           -> throwError (AppNotArrError fTy)
typeCheck (Ifz g t e) = do
    assertTy Nat g
    tTy <- typeCheck t
    assertTy (Arr Nat tTy) e
    pure tTy
typeCheck (Lam (Name n _) aTy bind) = do
    gen <- view (field @"teGen")
    a <- lift (gen n)
    bTy <- instantiateAndThen (field @"teTyMap") a aTy bind typeCheck
    pure (Arr aTy bTy)
typeCheck (Fix (Name n _) ty bind) = do
    gen <- view (field @"teGen")
    a <- lift (gen n)
    instantiateAndThen (field @"teTyMap") a ty bind (assertTy ty)
    pure ty
typeCheck (Suc e) = assertTy Nat e $> Nat
typeCheck Zero = pure Nat

-- Evaluation

data EvalError a =
      EvalMissingVarError a
    | NonNatGuardError (Exp a)
    | NonLamElseError (Exp a)
    | NonLamAppError (Exp a)
    deriving (Eq, Show)

data EvalEnv m a = EvalEnv
    { eeGen    :: VarGen m a
    , eeExpMap :: Map a (Exp a)
    } deriving (Generic)

type EvalT a m = FuncT (EvalEnv m a) () (EvalError a) m

bigStep :: (Monad m, Ord a) => Exp a -> EvalT a m (Exp a)
bigStep (Var a) = do
    expMap <- view (field @"eeExpMap")
    maybe (throwError (EvalMissingVarError a)) bigStep (M.lookup a expMap)
bigStep (App f x) = do
    fv <- bigStep f
    case fv of
        Lam (Name n _) _ bind -> do
            gen <- view (field @"eeGen")
            a <- lift (gen n)
            xv <- bigStep x
            instantiateAndThen (field @"eeExpMap") a xv bind bigStep
        -- TODO Allow Fix Lam too
        _ -> throwError (NonLamAppError fv)
bigStep (Ifz g t e) = do
    iv <- bigStep g
    case iv of
        Zero -> bigStep t
        Suc ev ->
            -- TODO need to evaluate e before pattern matching
            case e of
                Lam (Name n _) _ bind -> do
                    gen <- view (field @"eeGen")
                    a <- lift (gen n)
                    instantiateAndThen (field @"eeExpMap") a ev bind bigStep
                -- TODO Allow Fix Lam too
                _ -> throwError (NonLamElseError e)
        _ -> throwError (NonNatGuardError g)
bigStep v@Lam{} = pure v
bigStep v@Fix{} = pure v
bigStep v@Zero = pure v
bigStep (Suc e) = Suc <$> bigStep e

-- Closure conversion

data ConvEnv m a = ConvEnv
    { ceGen :: VarGen m a
    } deriving (Generic)

type ConvT a m = FuncT (ConvEnv m a) () Void m

closConv :: (Monad m, Eq a) => Exp a -> ConvT a m (ExpC a)
closConv (Var a) = pure (VarC a)
closConv (App l r) = AppC <$> closConv l <*> closConv r
closConv (Ifz g t e) = IfzC <$> closConv g <*> closConv t <*> closConv e
closConv (Suc e) = SucC <$> closConv e
closConv Zero = pure ZeroC
closConv (Lam i@(Name n _) ty b) = do
    gen <- view (field @"ceGen")
    a <- lift (gen n)
    (c, b') <- scopeRebind a b closConv
    pure (LamC i ty (VarC <$> c) b')
closConv (Fix i@(Name n _) ty b) = do
    gen <- view (field @"ceGen")
    a <- lift (gen n)
    (c, b') <- scopeRebind a b closConv
    pure (FixC i ty (VarC <$> c) b')

-- Lambda lifting

data LamLiftEnv m a = LamLiftEnv
    { lleGen :: VarGen m a
    } deriving (Generic)

data LamLiftError a =
      NonVarBoundCError (ExpC a)
    deriving (Eq, Show)

type LamLiftT a m = FuncT (LamLiftEnv m a) () (LamLiftError a) m

varOnlyC :: (Monad m, Traversable t) => t (ExpC a) -> LamLiftT a m (t a)
varOnlyC = traverse $ \case
    VarC a -> pure a
    e      -> throwError (NonVarBoundCError e)

lambdaLift :: (Monad m, Eq a) => ExpC a -> LamLiftT a m (ExpL a)
lambdaLift (VarC a) = pure (VarL a)
lambdaLift (AppC l r) = AppL <$> lambdaLift l <*> lambdaLift r
lambdaLift (IfzC g t e) = IfzL <$> lambdaLift g <*> lambdaLift t <*> lambdaLift e
lambdaLift (SucC e) = SucL <$> lambdaLift e
lambdaLift ZeroC = pure ZeroL
lambdaLift (LamC i@(Name n _) ty c b) = do
    gen <- view (field @"lleGen")
    a <- lift (gen n)
    c' <- varOnlyC c
    b' <- scopeRebindLam a c' b lambdaLift
    let bind = NRecL i ty (VarL <$> c') b'
    pure (LetL (V.singleton bind) (boundN 0))
lambdaLift (FixC i@(Name n _) ty c b) = do
    gen <- view (field @"lleGen")
    a <- lift (gen n)
    c' <- varOnlyC c
    b' <- scopeRebindLam a c' b lambdaLift
    let bind = RecL i ty (VarL <$> c') b'
    pure (LetL (V.singleton bind) (boundN 0))

-- FauxC conversion

data FauxError a =
      NonVarBoundLError (ExpL a)
    deriving (Eq, Show)

data FauxState a = FauxState
    { fsNextFunId :: FunId
    , fsTops      :: Vector (ExpFCTop a)
    } deriving (Eq, Show)

emptyFauxState :: FauxState a
emptyFauxState = FauxState { fsNextFunId = minBound, fsTops = V.empty }

type FauxT a m = FuncT () (FauxState a) (FauxError a) m

nextFunId :: Monad m => FauxT a m FunId
nextFunId = do
    s <- get
    let funId = fsNextFunId s
    put (s { fsNextFunId = succ funId })
    pure funId

tellTop :: Monad m => ExpFCTop a -> FauxT a m ()
tellTop t = modify (\s -> s { fsTops = V.snoc (fsTops s) t })

emitFun :: Monad m => Arity -> Scope Int ExpFC a -> FauxT a m FunId
emitFun ar body = do
    funId <- nextFunId
    tellTop (ExpFCTop funId ar body)
    pure funId

varOnlyL :: (Monad m, Traversable t) => t (ExpL a) -> FauxT a m (t a)
varOnlyL = traverse $ \case
    VarL a -> pure a
    e      -> throwError (NonVarBoundLError e)

nameL :: BindL a -> Text
nameL (RecL (Name n _) _ _ _)  = n
nameL (NRecL (Name n _) _ _ _) = n

fauxBindLift :: Monad m => (FunId -> ClosFC Text -> BindFC Text) -> Text -> ClosL Text -> Scope Int ExpL Text -> FauxT Text m (BindFC Text)
fauxBindLift mk n c b = do
    let arity = Arity (1 + V.length c)
    c' <- varOnlyL c
    b' <- scopeRebindLam n c' b faux
    funId <- emitFun arity b'
    pure (mk funId (VarFC <$> c'))

fauxBind :: Monad m => BindL Text -> FauxT Text m (BindFC Text)
fauxBind (NRecL i@(Name n _) ty c b) = fauxBindLift (NRecFC i ty) n c b
fauxBind (RecL i@(Name n _) ty c b)  = fauxBindLift (RecFC i ty) n c b

faux :: Monad m => ExpL Text -> FauxT Text m (ExpFC Text)
faux (VarL a) = pure (VarFC a)
faux (AppL l r) = AppFC <$> faux l <*> faux r
faux (IfzL g t e) = IfzFC <$> faux g <*> faux t <*> faux e
faux (SucL e) = SucFC <$> faux e
faux ZeroL = pure ZeroFC
faux (LetL xs b) = do
    let cs = nameL <$> xs
    xs' <- traverse fauxBind xs
    b' <- scopeRebindLet cs b faux
    pure (LetFC xs' b')
