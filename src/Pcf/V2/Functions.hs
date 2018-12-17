{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE Rank2Types #-}

module Pcf.V2.Functions where

import           Control.Lens          (Lens', assign, over, use, view)
import           Control.Monad         (unless)
import           Control.Monad.Except  (MonadError (..))
import           Control.Monad.Reader  (MonadReader (..))
-- import           Control.Monad.State.Strict (MonadState (..), modify)
import           Control.Monad.Trans   (MonadTrans (..))
import           Data.Functor          (($>))
import           Data.Generics.Product (field)
import           Data.Map              (Map)
import qualified Data.Map              as M
import           GHC.Generics          (Generic)
import           Pcf.Core.Func
import           Pcf.Core.Sub
import           Pcf.V2.Types

-- Utils

localMod :: MonadReader x m => (Lens' x y) -> (y -> y) -> m z -> m z
localMod lens mod = local (over lens mod)

-- Scope manipulation

instantiateAndThen :: (ThrowSub m, MonadReader x m, Functor f, Ord a) => (Lens' x (Map a w)) -> a -> w -> Binder n f a -> (Scope n f a -> m r) -> m r
instantiateAndThen lens a w b f = do
    s <- apply1 (pure a) b
    localMod lens (M.insert a w) (f s)

-- VarGen

type VarGen n m a = n -> m a

-- Typing

data TypeError a =
      CheckError Ty Ty
    | TypeMissingVarError a
    | AppNotArrError Ty
    | TypeWrapSubError SubError
    deriving (Generic, Eq, Show)

data TypeEnv n m a = TypeEnv
    { teGen   :: VarGen n m a
    , teTyMap :: Map a Ty
    } deriving (Generic)

type TypeT n a m = FuncT (TypeEnv n m a) () (TypeError a) m

instance Monad m => ThrowSub (TypeT n a m) where
    throwSub = throwError . TypeWrapSubError

assertTy :: (Monad m, Ord a) => Ty -> Exp n a -> TypeT n a m ()
assertTy t e = do
    u <- typeCheck e
    unless (u == t) (throwError (CheckError u t))

typeCheck :: (Monad m, Ord a) => Exp n a -> TypeT n a m Ty
typeCheck = foldScope sf where
    sf = closedFold free binder functor

    free a = do
        tyMap <- view (field @"teTyMap")
        maybe (throwError (TypeMissingVarError a)) pure (M.lookup a tyMap)

    binder b = do
        let ExpN (Name n ()) ty = binderInfo b
        gen <- view (field @"teGen")
        a <- lift (gen n)
        instantiateAndThen (field @"teTyMap") a ty b ((Arr ty <$>) . typeCheck)

    functor = \case
        App f x -> do
            fTy <- typeCheck f
            case fTy of
                Arr aTy bTy -> assertTy aTy x $> bTy
                _           -> throwError (AppNotArrError fTy)
        Ifz g t e -> do
            assertTy Nat g
            tTy <- typeCheck t
            assertTy (Arr Nat tTy) e
            pure tTy
        Suc e -> assertTy Nat e $> Nat
        Zero -> pure Nat

-- Evaluation

data EvalError n a =
    EvalMissingVarError a
  | NonNatGuardError (Exp n a)
  | NonLamElseError (Exp n a)
  | NonLamAppError (Exp n a)
  | EvalWrapSubError SubError
  deriving (Generic, Eq, Show)

data EvalEnv n m a = EvalEnv
  { eeGen    :: VarGen n m a
  , eeExpMap :: Map a (Exp n a)
  } deriving (Generic)

type EvalT n a m = FuncT (EvalEnv n m a) () (EvalError n a) m

instance Monad m => ThrowSub (EvalT n a m) where
    throwSub = throwError . EvalWrapSubError

bigStep :: (Monad m, Ord a) => Exp n a -> EvalT n a m (Exp n a)
bigStep = foldScope sf where
    sf = closedFold free binder functor

    free a = do
        expMap <- view (field @"eeExpMap")
        maybe (throwError (EvalMissingVarError a)) pure (M.lookup a expMap)

    binder = pure . boundScope

    functor = \case
        App f x -> do
            fv <- bigStep f
            case matchBinder fv of
                Just b -> do
                    let n = nameKey (expName (binderInfo b))
                    gen <- view (field @"eeGen")
                    a <- lift (gen n)
                    xv <- bigStep x
                    instantiateAndThen (field @"eeExpMap") a xv b bigStep
                Nothing -> throwError (NonLamAppError fv)
        Ifz g t e -> do
            iv <- bigStep g
            case matchFunctor iv of
                Just Zero -> bigStep t
                Just (Suc ev) -> do
                    e' <- bigStep e
                    case matchBinder e' of
                        Just b -> do
                            let n = nameKey (expName (binderInfo b))
                            gen <- view (field @"eeGen")
                            a <- lift (gen n)
                            instantiateAndThen (field @"eeExpMap") a ev b bigStep
                        Nothing -> throwError (NonLamElseError e')
                _ -> throwError (NonNatGuardError g)
        Suc e -> wrapScope . Suc <$> bigStep e
        Zero -> pure (liftScope Zero)
