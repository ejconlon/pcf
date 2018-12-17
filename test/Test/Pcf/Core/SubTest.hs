module Test.Pcf.Core.SubTest where

import Control.Monad.Identity (Identity (..))
import Data.Vector            as V
import Pcf.Core.Sub
import Pcf.Core.Sub.Internal
import Test.Pcf.Assertions    ((@/=))
import Test.Tasty
import Test.Tasty.HUnit

type BareScope = Scope (NameOnly Char) Identity Char
type BareBinder = Binder (NameOnly Char) Identity Char

absBound :: Char -> BareScope -> BareBinder
absBound a = abstract1 (Name a ()) a

absScope :: Char -> BareScope -> BareScope
absScope a = boundScope . absBound a

boundVar :: Int -> BareScope
boundVar = Scope . ScopeB

test_sub :: TestTree
test_sub =
    let svar = pure 'x' :: BareScope
        sbound = boundVar 0 :: BareScope
        bfree = absBound 'y' (pure 'x') :: BareBinder
        sfree = boundScope bfree
        bfree2 = absBound 'z' (absScope 'y' (pure 'x')) :: BareBinder
        sfree2 = boundScope bfree2
        bid = absBound 'x' (pure 'x') :: BareBinder
        sid = boundScope bid
        bwonky = absBound 'x' (boundVar 0) :: BareBinder
        swonky = boundScope bwonky
        bconst = absBound 'x' (absScope 'y' (pure 'x')) :: BareBinder
        sconst = boundScope bconst
        bflip = absBound 'x' (absScope 'y' (pure 'y')) :: BareBinder
        sflip = boundScope bflip
        svar2 = pure 'e' :: BareScope
        swonky2 = absScope 'x' svar2 :: BareScope

        testEq = testCase "eq" $ do
            svar @?= svar
            svar @/= svar2
            sid @?= absScope 'x' (pure 'x')
            sid @?= absScope 'y' (pure 'y')
            sid @/= absScope 'x' (pure 'y')
            sid @/= absScope 'y' (pure 'x')
            sid @/= svar

        testFreeVars = testCase "free vars" $ do
            scopeFreeVars sid @?= V.empty
            scopeFreeVars sconst @?= V.empty
            scopeFreeVars sfree @?= V.singleton 'x'
            scopeFreeVars sfree2 @?= V.singleton 'x'
            scopeFreeVars svar @?= V.singleton 'x'
            scopeFreeVars svar2 @=? V.singleton 'e'

        testAbstract = testCase "abstract" $ do
            svar @?= (Scope (ScopeF 'x') :: BareScope)
            sbound @?= (Scope (ScopeB 0) :: BareScope)
            sfree @?= (Scope (ScopeA (UnderBinder 1 (Name 'y' ()) (Scope (ScopeF 'x')))) :: BareScope)
            sfree2 @?= (Scope (ScopeA (UnderBinder 1 (Name 'z' ()) (Scope (ScopeA (UnderBinder 1 (Name 'y' ()) (Scope (ScopeF 'x'))))))) :: BareScope)
            sid @?= (Scope (ScopeA (UnderBinder 1 (Name 'x' ()) (Scope (ScopeB 0)))) :: BareScope)
            swonky @?= (Scope (ScopeA (UnderBinder 1 (Name 'x' ()) (Scope (ScopeB 1)))) :: BareScope)
            sconst @?= (Scope (ScopeA (UnderBinder 1 (Name 'x' ()) (Scope (ScopeA (UnderBinder 1 (Name 'y' ()) (Scope (ScopeB 1))))))) :: BareScope)
            sflip @?= (Scope (ScopeA (UnderBinder 1 (Name 'x' ()) (Scope (ScopeA (UnderBinder 1 (Name 'y' ()) (Scope (ScopeB 0))))))) :: BareScope)

        testInstantiate = testCase "instantiate" $ do
            instantiate1 svar2 svar @?= svar
            instantiate1 svar2 sbound @?= svar2
            instantiate1 svar2 sid @?= sid
            instantiate1 svar2 swonky @?= swonky2

        testApply = testCase "apply" $ do
            runSub (apply1 svar2 bid) @?= Right svar2
            runSub (apply1 svar2 bwonky) @?= Right sbound
            runSub (apply1 svar2 bconst) @?= Right swonky2
            runSub (apply1 svar2 bflip) @?= Right sid

        testVarSub = testCase "var sub" $ do
            (svar >>= const svar2) @?= svar2
            (sfree >>= const svar2) @?= absScope 'y' svar2
            (sfree2 >>= const svar2) @?= absScope 'c' (absScope 'd' svar2)

        testIdSub = testCase "id sub" $ do
            (svar >>= const sid) @?= sid
            (sfree >>= const sid) @?= absScope 'y' sid
            (sfree2 >>= const sid) @?= absScope 'c' (absScope 'd' sid)

    in testGroup "sub" [testEq, testFreeVars, testAbstract, testInstantiate, testApply, testVarSub, testIdSub]
