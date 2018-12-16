module Pcf.V2.Functions where

import           Control.Lens          (Lens', assign, over, use, view)
import           Control.Monad         (unless)
import           Control.Monad.Except  (MonadError (..))
-- import           Control.Monad.Reader       (MonadReader (..))
-- import           Control.Monad.State.Strict (MonadState (..), modify)
-- import           Control.Monad.Trans        (MonadTrans (..))
import           Data.Functor          (($>))
import           Data.Generics.Product (field)
import           Data.Map              (Map)
import qualified Data.Map              as M
import           GHC.Generics          (Generic)
import           Pcf.Core.Func
import           Pcf.Core.Sub
import           Pcf.V2.Types

-- VarGen

type VarGen n m a = n -> m a

-- Typing

data TypeError a =
      CheckError Ty Ty
    | TypeMissingVarError a
    | AppNotArrError Ty
    | TySubErr
    deriving (Eq, Show)

data TypeEnv n m a = TypeEnv
    { teGen   :: VarGen n m a
    , teTyMap :: Map a Ty
    } deriving (Generic)

type TypeT n a m = FuncT (TypeEnv n m a) () (TypeError a) m

-- tySF :: (Monad m, Ord a) => ScopeFold (ExpF n) a (TypeT n a m Ty)
-- tySF = ScopeFold free binder functor where
--     free a = do
--         tyMap <- view (field @"teTyMap")
--         maybe (throwError (TypeMissingVarError a)) pure (M.lookup a tyMap)

--     binder = undefined

--     functor = undefined

assertTy :: (Monad m, Ord a) => Ty -> Exp n a -> TypeT n a m ()
assertTy t e = do
    u <- typeCheck e
    unless (u == t) (throwError (CheckError u t))

typeCheck :: (Monad m, Ord a) => Exp n a -> TypeT n a m Ty
typeCheck = undefined

-- runNiceScopeFold free functor (throwError TySubErr) where
--     free a = do
--         tyMap <- view (field @"teTyMap")
--         maybe (throwError (TypeMissingVarError a)) pure (M.lookup a tyMap)

--     functor = \case
--         Suc e -> assertTy Nat e $> Nat
--         Zero -> pure Nat
