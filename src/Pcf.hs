module Pcf where

import Bound
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Gen
import Control.Monad.Trans.Maybe

main :: IO ()
main = putStrLn "hello, world"

data Ty =
    Arr Ty Ty
  | Nat deriving (Show, Eq)

data Exp a =
    V a
  | App (Exp a) (Exp a)
  | Ifz (Exp a) (Exp a) (Scope () Exp a)
  | Lam Ty (Scope () Exp a)
  | Fix Ty (Scope () Exp a)
  | Suc (Exp a)
  | Zero
  deriving (Functor, Foldable, Traversable)

type TyM a = MaybeT (Gen a)

assertTy :: (Enum a, Ord a) => Map a Ty -> Exp a -> Ty -> TyM a ()
assertTy = undefined

typeCheck :: (Enum a, Ord a) => Map a Ty -> Exp a -> TyM a Ty
typeCheck = undefined