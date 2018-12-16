module Test.Pcf.SubTest where

import Control.Monad.Identity (Identity (..))
import Data.Vector            as V
import Pcf.Sub
import Pcf.Sub.Internal
import Test.Pcf.Assertions    ((@/=))
import Test.Tasty
import Test.Tasty.HUnit

-- Exp

data ExpF n a =
      App a a
    | Ifz a a a
    | Zero
    | Suc a
    | Lam (NameOnly n) a
    deriving (Eq, Show, Functor, Foldable, Traversable)

type Exp n a = Scope (ExpF n) a

lam' :: Eq a => n -> a -> Exp n a -> Exp n a
lam' n a = wrapScope . Lam (Name n ()) . boundScope . abstract1 a

lam :: Eq n => n -> Exp n n -> Exp n n
lam n = lam' n n

-- Test gen

type BareScope = Scope Identity Char
type BareBinder = Binder Identity Char

type LamCtor f a = a -> Scope f a -> Scope f a

bareLamCtor :: Eq a => LamCtor Identity a
bareLamCtor a = boundScope . abstract1 a

expLamCtor :: Eq a => LamCtor (ExpF a) a
expLamCtor = lam

-- Core tests

boundVar :: Int -> Scope f a
boundVar = Scope . ScopeB

test_core :: TestTree
test_core =
    let svar = pure 'x' :: BareScope
        sbound = boundVar 0 :: BareScope
        bfree = abstract1 'y' (pure 'x') :: BareBinder
        sfree = boundScope bfree
        bfree2 = abstract1 'z' (bareLamCtor 'y' (pure 'x')) :: BareBinder
        sfree2 = boundScope bfree2
        bid = abstract1 'x' (pure 'x') :: BareBinder
        sid = boundScope bid
        bwonky = abstract1 'x' (boundVar 0) :: BareBinder
        swonky = boundScope bwonky
        bconst = abstract1 'x' (bareLamCtor 'y' (pure 'x')) :: BareBinder
        sconst = boundScope bconst
        bflip = abstract1 'x' (bareLamCtor 'y' (pure 'y')) :: BareBinder
        sflip = boundScope bflip

        testAbstract = testCase "abstract" $ do
            svar @?= (Scope (ScopeF 'x') :: BareScope)
            sbound @?= (Scope (ScopeB 0) :: BareScope)
            sfree @?= (Scope (ScopeA (UnderBinder 1 (Scope (ScopeF 'x')))) :: BareScope)
            sfree2 @?= (Scope (ScopeA (UnderBinder 1 (Scope (ScopeA (UnderBinder 1 (Scope (ScopeF 'x'))))))) :: BareScope)
            sid @?= (Scope (ScopeA (UnderBinder 1 (Scope (ScopeB 0)))) :: BareScope)
            swonky @?= (Scope (ScopeA (UnderBinder 1 (Scope (ScopeB 1)))) :: BareScope)
            sconst @?= (Scope (ScopeA (UnderBinder 1 (Scope (ScopeA (UnderBinder 1 (Scope (ScopeB 1))))))) :: BareScope)
            sflip @?= (Scope (ScopeA (UnderBinder 1 (Scope (ScopeA (UnderBinder 1 (Scope (ScopeB 0))))))) :: BareScope)

        svar2 = pure 'e' :: BareScope
        swonky2 = bareLamCtor 'x' svar2 :: BareScope

        testInstantiate = testCase "instantiate" $ do
            instantiate1 svar2 svar @?= svar
            instantiate1 svar2 sbound @?= svar2
            instantiate1 svar2 sid @?= sid
            instantiate1 svar2 swonky @?= swonky2

        testApply = testCase "apply" $ do
            1 @?= 1
            apply1 svar2 bid @?= pure svar2
            apply1 svar2 bwonky @?= pure sbound
            apply1 svar2 bconst @?= pure swonky2
            apply1 svar2 bflip @?= pure sid

    in testGroup "sub - core" [testAbstract, testInstantiate, testApply]

-- Comprehensive tests

makeTests :: (Functor f, Foldable f, Eq (f (Scope f Char)), Show (f (Scope f Char))) => String -> LamCtor f Char -> TestTree
makeTests name lamb =
    let sid = lamb 'x' (pure 'x')
        sconst = lamb 'a' (lamb 'b' (pure 'a'))
        sfree = lamb 'y' (pure 'z')
        sfree2 = lamb 'c' (lamb 'd' (pure 'e'))
        svar = pure 'w'
        svar2 = pure 'u'

        testEq = testCase "eq" $ do
            svar @?= svar
            svar @/= svar2
            sid @?= lamb 'x' (pure 'x')
            sid @?= lamb 'y' (pure 'y')
            sid @/= lamb 'x' (pure 'y')
            sid @/= lamb 'y' (pure 'x')
            sid @/= svar

        testFreeVars = testCase "free vars" $ do
            scopeFreeVars sid @?= V.empty
            scopeFreeVars sconst @?= V.empty
            scopeFreeVars sfree @?= V.singleton 'z'
            scopeFreeVars sfree2 @?= V.singleton 'e'
            scopeFreeVars svar @?= V.singleton 'w'
            scopeFreeVars svar2 @=? V.singleton 'u'

        testVarSub = testCase "var sub" $ do
            (svar >>= const svar2) @?= svar2
            (sfree >>= const svar2) @?= lamb 'y' svar2
            (sfree2 >>= const svar2) @?= lamb 'c' (lamb 'd' svar2)

        testIdSub = testCase "id sub" $ do
            (svar >>= const sid) @?= sid
            (sfree >>= const sid) @?= lamb 'y' sid
            (sfree2 >>= const sid) @?= lamb 'c' (lamb 'd' sid)

    in testGroup name [testEq, testFreeVars, testVarSub, testIdSub]

test_bare :: TestTree
test_bare = makeTests "sub - bare" bareLamCtor

test_exp :: TestTree
test_exp = makeTests "sub - exp" expLamCtor
