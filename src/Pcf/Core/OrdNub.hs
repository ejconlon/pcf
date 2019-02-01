module Pcf.Core.OrdNub where

import Control.Monad.State (State, evalState, get, put)
import Data.Foldable (traverse_)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Sequence (Seq (..))
import qualified Data.Sequence as Seq

data NubState a = NubState (Set a) (Seq a) deriving (Show, Eq)

emptyNubState :: NubState a
emptyNubState = NubState S.empty Seq.empty

addNub :: Ord a => a -> State (NubState a) ()
addNub a = do
    NubState set seq <- get
    if S.member a set
        then pure ()
        else put (NubState (S.insert a set) (seq :|> a))

ordNub :: (Foldable f, Ord a) => f a -> Seq a
ordNub as = evalState ((\(NubState _ s) -> s) <$> (traverse_ addNub as *> get)) emptyNubState
