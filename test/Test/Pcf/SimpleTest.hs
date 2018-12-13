{-# LANGUAGE Rank2Types #-}

module Test.Pcf.SimpleTest where

import           Bound               as B
import           Bound.Name          (Name (..))
import qualified Data.Set            as S
import           Pcf.Functions
import           Pcf.Types           (Exp (..), Ty (..))
import           Test.Pcf.Assertions ((@/=))
import           Test.Tasty
import           Test.Tasty.HUnit

lamEq :: (forall a. a -> Exp a) -> IO ()
lamEq body = do
    let lamInt = lam "x" 42 Nat (body 42)
        lamText = lam' "x" Nat (body "x")
        lamBound = Lam (Name "x" ()) Nat (B.Scope (body (B.B ())))
    lamInt @?= lamBound
    lamText @?= lamBound
    let fixInt = fix "x" 42 Nat (body 42)
        fixText = fix' "x" Nat (body "x")
        fixBound = Fix (Name "x" ()) Nat (B.Scope (body (B.B ())))
    fixInt @?= fixBound
    fixText @?= fixBound
    lam' "x" Nat (body "x") @?= lam' "y" Nat (body "y")

test_expEq :: TestTree
test_expEq = testCase "exp eq" $ do
    Var 0 @?= Var 0
    Var 0 @/= Var 1
    Nat @?= Nat
    Nat @/= Arr Nat Nat
    lamEq (Suc . Var)

test_freeVars :: TestTree
test_freeVars = testCase "free vars" $ do
    freeVars (Var 0) @?= S.singleton 0
    freeVars (Suc (Var 0)) @?= S.singleton 0
    freeVars (lam' "x" Nat (Var "x")) @?= S.empty
    freeVars (lam' "x" Nat (Var "y")) @?= S.singleton "y"
