module Pcf.Core.Expand where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (ask, local)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)
import Pcf.Core.Func (Func, runFunc)
import Pcf.Core.Util (trabind)

data ExpandError a = ExpandCycleError a (Seq a) deriving (Generic, Show, Eq)

data ExpandEnv f a = ExpandEnv
    { eeMap :: Map a (f a)
    , eeSet :: Set a
    , eeSeq :: Seq a
    } deriving (Generic, Show, Eq)

type Expand f a b = Func (ExpandEnv f a) () (ExpandError a) b

runExpand :: Expand f a b -> Map a (f a) -> Either (ExpandError a) b
runExpand e m = fst (runFunc e (ExpandEnv m S.empty Seq.empty) ())

expand' :: (Monad f, Traversable f, Ord a) => a -> Expand f a (f a)
expand' a = do
    ExpandEnv map set seq <- ask
    if S.member a set
        then throwError (ExpandCycleError a seq)
        else do
            case M.lookup a map of
                Just fa ->
                    let set' = S.insert a set
                        seq' = seq :|> a
                    in local (const (ExpandEnv map set' seq')) (trabind fa expand')
                Nothing -> pure (pure a)

expand :: (Monad f, Traversable f, Ord a) => Map a (f a) -> a -> Either (ExpandError a) (f a)
expand m a = runExpand (expand' a) m
