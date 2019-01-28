module Pcf.Core.Func where

import Control.Monad.Except       (ExceptT, MonadError (..), runExceptT)
import Control.Monad.Identity     (Identity (..))
import Control.Monad.Reader       (MonadReader (..), ReaderT, runReaderT)
import Control.Monad.State.Strict (MonadState (..), StateT, modify, runStateT)
import Control.Monad.Trans        (MonadTrans (..))

newtype FuncT r s e m a = FuncT { unFuncT :: ReaderT r (ExceptT e (StateT s m)) a }
    deriving (Functor, Applicative, Monad, MonadReader r, MonadState s, MonadError e)

type Func r s e a = FuncT r s e Identity a

instance MonadTrans (FuncT r s e) where
    lift = FuncT . lift . lift . lift

runFuncT :: FuncT r s e m a -> r -> s -> m (Either e a, s)
runFuncT act env = runStateT (runExceptT (runReaderT (unFuncT act) env))

runFunc :: Func r s e a -> r -> s -> (Either e a, s)
runFunc act env st = runIdentity (runFuncT act env st)
