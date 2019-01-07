{-# LANGUAGE Rank2Types #-}

module Pcf.V3.Functions where

import           Bound                 (abstract, instantiate1, makeBound)
import           Control.Applicative   (empty)
import           Control.Lens          (view)
import           Control.Monad         (unless)
import           Control.Monad.Except  (MonadError, throwError)
import           Control.Monad.Reader  (MonadReader)
import           Data.Foldable         (traverse_)
import           Data.Generics.Product (field)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as M
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Vector           (Vector)
import qualified Data.Vector           as V
import           GHC.Generics          (Generic)
import           Pcf.Core.BoundCrazy   (instantiateM)
import           Pcf.Core.BoundUtil    (localMod)
import           Pcf.Core.Func         (FuncT)
import           Pcf.V3.Types

-- Utils

insertAll :: (Foldable t, Ord a) => t (a, b) -> Map a b -> Map a b
insertAll nts m0 = foldl (\m (n, t) -> M.insert n t m) m0 nts

-- Smart constructors

lam0 :: Vector (Name, Type0) -> Exp0 Name -> Exp0 Name
lam0 nts = let ns = fst <$> nts in Lam0 nts . abstract (flip V.elemIndex ns)

-- Typing

data TypeError =
      TypeCheckError Type0 Type0
    | TypeMissingVarError Name
    | TypeUnboundVar Int
    | TypeTooManyArgs Int Int
    | TypeNotLambda
    | TypeNotCont
    deriving (Eq, Show)

data TypeEnv = TypeEnv
    { teTyMap :: Map Name Type0
    } deriving (Generic, Eq, Show)

type TypeC m = (MonadReader TypeEnv m, MonadError TypeError m)
type TypeT m a = FuncT TypeEnv () TypeError m a

typeProof :: Monad m => (forall n. TypeC n => n a) -> TypeT m a
typeProof = id

checkType0 :: TypeC m => Type0 -> Exp0 Name -> m ()
checkType0 t e = do
    u <- inferType0 e
    unless (u == t) (throwError (TypeCheckError u t))

inferType0 :: TypeC m => Exp0 Name -> m Type0
inferType0 (Var0 n) = do
    tyMap <- view (field @"teTyMap")
    maybe (throwError (TypeMissingVarError n)) pure (M.lookup n tyMap)
inferType0 (Lam0 nts b) = do
    let k z = maybe (throwError (TypeUnboundVar z)) (pure . Var0 . fst) (nts V.!? z)
    e <- instantiateM k b
    et <- localMod (field @"teTyMap") (insertAll nts) (inferType0 e)
    let ts = snd <$> nts
    pure (TyArr0 ts et)
inferType0 (Call0 e xs) = do
    et <- inferType0 e
    case et of
        TyArr0 ats rty -> do
            let xlen = V.length xs
                alen = V.length ats
            if xlen > alen
                then throwError (TypeTooManyArgs xlen alen)
                else do
                    traverse_ (uncurry checkType0) (V.zip ats xs)
                    pure (if xlen == alen then rty else (TyArr0 (V.drop xlen ats) rty))
        _ -> throwError TypeNotLambda
inferType0 Bool0{} = pure TyBool0
inferType0 (If0 g t e) = do
    checkType0 TyBool0 g
    tt <- inferType0 t
    checkType0 tt e
    pure tt
inferType0 (Control0 n t b) = do
    let e = instantiate1 (Var0 n) b
    localMod (field @"teTyMap") (M.insert n (TyCont0 t)) (checkType0 t e)
    pure t
inferType0 (Throw0 c e) = do
    ct <- inferType0 c
    case ct of
        TyCont0 t -> checkType0 t e >> pure t
        _         -> throwError TypeNotCont

-- Evaluation

-- data EvalError =
--     EvalBoom
--     deriving (Eq, Show)

-- data EvalEnv = EvalEnv
--     { eeTmMap :: Map Name (Exp0 Name)
--     } deriving (Generic, Eq, Show)

-- type EvalC m = (MonadReader EvalEnv m, MonadError EvalError m)
-- type EvalT m a = FuncT EvalEnv () EvalError m a

-- evalProof :: Monad m => (forall n. EvalC n => n a) -> EvalT m a
-- evalProof = id

-- eval0 :: EvalC m => Exp0 Name -> m (Exp0 Name)
-- eval0 (Var0 n) = do
--     tmMap <- view (field @"eeTmMap")
--     maybe undefined pure (M.lookup n tmMap)
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
