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

instantiateAndThen :: (MonadReader x m, Functor f, Ord a) => (Lens' x (Map a w)) -> a -> w -> Binder n f a -> m r -> (Scope n f a -> m r) -> m r
instantiateAndThen lens a w b z f =
    case apply1 (pure a) b of
        Nothing -> z
        Just s -> localMod lens (M.insert a w) (f s)

-- VarGen

type VarGen n m a = n -> m a

-- Typing

data TypeError a =
      CheckError Ty Ty
    | TypeMissingVarError a
    | AppNotArrError Ty
    | TyUnboundErr Int
    | TySubErr
    deriving (Eq, Show)

data TypeEnv n m a = TypeEnv
    { teGen   :: VarGen n m a
    , teTyMap :: Map a Ty
    } deriving (Generic)

type TypeT n a m = FuncT (TypeEnv n m a) () (TypeError a) m

assertTy :: (Monad m, Ord a) => Ty -> Exp n a -> TypeT n a m ()
assertTy t e = do
    u <- typeCheck e
    unless (u == t) (throwError (CheckError u t))

tySF :: (Monad m, Ord a) => ExpFold n a (TypeT n a m Ty)
tySF = ScopeFold bound free binder functor where
    bound = throwError . TyUnboundErr

    free a = do
        tyMap <- view (field @"teTyMap")
        maybe (throwError (TypeMissingVarError a)) pure (M.lookup a tyMap)

    binder b = do
        let ExpN (Name n ()) ty = binderInfo b
        gen <- view (field @"teGen")
        a <- lift (gen n)
        instantiateAndThen (field @"teTyMap") a ty b (throwError TySubErr) ((Arr ty <$>) . typeCheck)

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

typeCheck :: (Monad m, Ord a) => Exp n a -> TypeT n a m Ty
typeCheck = foldScope tySF
