{-# LANGUAGE Rank2Types #-}

module Pcf.V3.Functions where

import           Bound                 (Scope, abstract, instantiate1, makeBound)
import           Control.Applicative   (empty)
import           Control.Lens          (assign, modifying, use, view)
import           Control.Monad         (unless)
import           Control.Monad.Except  (MonadError, throwError)
import           Control.Monad.Reader  (MonadReader, ask)
import           Control.Monad.State   (MonadState)
import           Data.Foldable         (traverse_)
import           Data.Generics.Product (field)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as M
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Sequence         ((|>), (!?), Seq (..))
import qualified Data.Sequence         as Seq
import           GHC.Generics          (Generic)
import           Pcf.Core.BoundCrazy   (instantiateM)
import           Pcf.Core.BoundUtil    (localMod)
import           Pcf.Core.Func         (FuncT)
import           Pcf.V3.Types

-- Utils

insertAll :: (Foldable t, Ord a) => t (a, b) -> Map a b -> Map a b
insertAll nts m0 = foldl (\m (n, t) -> M.insert n t m) m0 nts

-- Smart constructors

lam0 :: Seq (Name, Type0) -> Exp0 Name -> Exp0 Name
lam0 nts = let ns = fst <$> nts in Lam0 nts . abstract (flip Seq.elemIndexR ns)

-- Stuff

data EnvError r e = EnvError
    { eeEnv :: r
    , eeError :: e
    } deriving (Generic, Eq, Show)

throwEnvError :: (MonadReader r m, MonadError (EnvError r e) m) => e -> m z
throwEnvError error = do
    env <- ask
    throwError (EnvError env error)

-- Typing

data TypeError =
      TypeCheckError Type0 Type0
    | TypeMissingVarError Name
    | TypeUnboundVar Int
    | TypeTooManyArgs Int Int
    | TypeNotLambda
    | TypeNotCont
    deriving (Generic, Eq, Show)

data TypeEnv = TypeEnv
    { teTyMap :: Map Name Type0
    , tePath :: Path0
    } deriving (Generic, Eq, Show)

type FullTypeError = EnvError TypeEnv TypeError

type TypeC m = (MonadReader TypeEnv m, MonadError FullTypeError m)
type TypeT m a = FuncT TypeEnv () FullTypeError m a

typeProof :: Monad m => (forall n. TypeC n => n a) -> TypeT m a
typeProof = id

checkType0 :: TypeC m => Type0 -> Exp0 Name -> m ()
checkType0 t e = do
    u <- inferType0 e
    unless (u == t) (throwEnvError (TypeCheckError u t))

withDir0 :: MonadReader TypeEnv m => Dir0 -> m z -> m z
withDir0 d = localMod (field @"tePath") (flip (|>) d)

inferTyArr0 :: TypeC m => Exp0 Name -> m (Seq Type0, Type0)
inferTyArr0 e = do
    et <- inferType0 e
    case et of
        TyArr0 ats rty -> pure (ats, rty)
        _ -> throwEnvError TypeNotLambda

inferTyCont0 :: TypeC m => Exp0 Name -> m Type0
inferTyCont0 e = do
    et <- inferType0 e
    case et of
        TyCont0 t -> pure t
        _ -> throwEnvError TypeNotCont

izipWithM_ :: (Int -> a -> b -> m ()) -> Seq a -> Seq b -> m ()
izipWithM_ = undefined

inferType0 :: TypeC m => Exp0 Name -> m Type0
inferType0 (Var0 n) = do
    tyMap <- view (field @"teTyMap")
    maybe (throwEnvError (TypeMissingVarError n)) pure (M.lookup n tyMap)
inferType0 (Lam0 nts b) = do
    let k z = maybe (throwEnvError (TypeUnboundVar z)) (pure . Var0 . fst) (nts !? z)
    et <- withDir0 DirLamBody0 $ do
        e <- instantiateM k b
        localMod (field @"teTyMap") (insertAll nts) (inferType0 e)
    let ts = snd <$> nts
    pure (TyArr0 ts et)
inferType0 (Call0 e xs) = do
    (ats, rty) <- withDir0 DirCallFun0 (inferTyArr0 e)
    let xlen = Seq.length xs
        alen = Seq.length ats
    if xlen > alen
        then throwEnvError (TypeTooManyArgs xlen alen)
        else do
            izipWithM_ (\i at x -> withDir0 (DirCallArg0 i) (checkType0 at x)) ats xs
            pure (if xlen == alen then rty else (TyArr0 (Seq.drop xlen ats) rty))
inferType0 Bool0{} = pure TyBool0
inferType0 (If0 g t e) = do
    withDir0 DirIfGuard0 (checkType0 TyBool0 g)
    tt <- withDir0 DirIfThen0 (inferType0 t)
    withDir0 DirIfElse0 (checkType0 tt e)
    pure tt
inferType0 (Control0 n t b) = do
    let e = instantiate1 (Var0 n) b
    localMod (field @"teTyMap") (M.insert n (TyCont0 t)) (withDir0 DirControlBody0 (checkType0 t e))
    pure t
inferType0 (Throw0 c e) = do
    t <- withDir0 DirThrowFun0 (inferTyCont0 c)
    withDir0 DirThrowArg0 (checkType0 t e)
    pure t

-- Evaluation

data Kont0 a =
    KontTop0
  | KontCallFun0 (Seq (Exp0 a)) (Kont0 a)
  | KontCallArg0 (Exp0 a) (Seq (Exp0 a)) (Seq (Exp0 a)) (Kont0 a)
  | KontIf0 (Exp0 a) (Exp0 a) (Kont0 a)
  | KontThrowFun0 (Exp0 a) (Kont0 a)
  | KontThrowArg0 (Exp0 a) (Kont0 a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

data EvalError =
      EvalBoom
    | EvalMissingVarError Name
    | EvalTooManyArgs Int Int
    | EvalNotLambda
    deriving (Generic, Eq, Show)

data EvalEnv = EvalEnv
    { eeTmMap :: Map Name (Exp0 Name)
    } deriving (Generic, Eq, Show)

data EvalState = EvalState
    { esKont :: Kont0 Name
    } deriving (Generic, Eq, Show)

type EvalC m = (MonadReader EvalEnv m, MonadState EvalState m, MonadError EvalError m)
type EvalT m a = FuncT EvalEnv EvalState EvalError m a

evalProof :: Monad m => (forall n. EvalC n => n a) -> EvalT m a
evalProof = id

call0 :: EvalC m => Seq Name -> Seq (Exp0 Name) -> Scope Int Exp0 Name -> m (Exp0 Name)
call0 = undefined

step0 :: EvalC m => Exp0 Name -> m (Maybe (Exp0 Name))
step0 e =
    case e of
        Var0 n -> do
            tmMap <- view (field @"eeTmMap")
            case M.lookup n tmMap of
                Nothing -> throwError (EvalMissingVarError n)
                Just e' -> pure (Just e')
        Call0 e xs -> do
            modifying (field @"esKont") (KontCallFun0 xs)
            pure (Just e)
        If0 g t e -> do
            modifying (field @"esKont") (KontIf0 t e)
            pure (Just e)
        _ -> do
            k <- use (field @"esKont")
            case k of
                KontTop0 -> pure Nothing
                KontCallFun0 xs n ->
                    case e of
                        Lam0 nts b ->
                            case xs of
                                Seq.Empty ->
                                    Just <$> (call0 (fst <$> nts) Seq.empty b)
                                x :<| xs -> do
                                    modifying (field @"esKont") (KontCallArg0 e Seq.empty xs)
                                    pure (Just x)
                        _ -> throwError (EvalNotLambda)
                _ -> throwError (EvalBoom)
-- eval0 e@Lam0{} = pure e
-- eval0 (Call0 e xs) = do
--     xs' <- traverse eval0 xs
--     e' <- eval0 e
--     case e' of
--         Lam0 ns b -> undefined
--         _ -> undefined
-- eval0 e@Bool0{} = pure e
-- eval0 (If0 g t e) = do
--     g' <- eval0 g
--     case g' of
--         Bool0 True -> eval0 t
--         Bool0 False -> eval0 e
--         _ -> undefined
-- eval0 (Control0 n t b) = undefined
-- eval0 (Throw0 c e) = undefined
