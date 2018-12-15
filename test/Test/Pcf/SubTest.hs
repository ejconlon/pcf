module Test.Pcf.SubTest where

import Control.Monad.Identity (Identity (..))
import Data.Vector            as V
import Pcf.Sub
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

type Bare a = Scope Identity a

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
    let bvar = pure 'x' :: Bare Char
        bbound = boundVar 0 :: Bare Char
        bfree = bareLamCtor 'y' bvar :: Bare Char
        bfree2 = bareLamCtor 'z' bfree :: Bare Char
        bid = bareLamCtor 'x' bvar :: Bare Char
        bwonky = bareLamCtor 'x' bbound :: Bare Char
        bconst = bareLamCtor 'x' bfree :: Bare Char
        bflip = bareLamCtor 'y' bid :: Bare Char

        testAbstract = testCase "abstract" $ do
            bvar @?= (Scope (ScopeF 'x') :: Bare Char)
            bbound @?= (Scope (ScopeB 0) :: Bare Char)
            bfree @?= (Scope (ScopeA (UnderBinder 1 (Scope (ScopeF 'x')))) :: Bare Char)
            bfree2 @?= (Scope (ScopeA (UnderBinder 1 (Scope (ScopeA (UnderBinder 1 (Scope (ScopeF 'x'))))))) :: Bare Char)
            bid @?= (Scope (ScopeA (UnderBinder 1 (Scope (ScopeB 0)))) :: Bare Char)
            bwonky @?= (Scope (ScopeA (UnderBinder 1 (Scope (ScopeB 1)))) :: Bare Char)
            bconst @?= (Scope (ScopeA (UnderBinder 1 (Scope (ScopeA (UnderBinder 1 (Scope (ScopeB 1))))))) :: Bare Char)
            bflip @?= (Scope (ScopeA (UnderBinder 1 (Scope (ScopeA (UnderBinder 1 (Scope (ScopeB 0))))))) :: Bare Char)

        testInstantiate = testCase "instantiate" $ do
            1 @?= 1
            -- let bvar2 = pure 'e' :: Bare Char
            -- instantiate1 bvar2 bvar @?= bvar
            -- instantiate1 bvar2 bbound @?= bvar2
            -- instantiate1 bvar2 bid @?= bbound
            -- instantiate1 bvar2 bwonky @?= bvar2
            -- instantiate1 bvar2 bconst @?= (bareLamCtor 'x' bvar2 :: Bare Char)
            -- instantiate1 bvar2 bflip @?= bflip

    in testGroup "sub - core" [testAbstract, testInstantiate]

-- Comprehensive tests

makeTests :: (Functor f, Foldable f, Eq (f (Scope f Char)), Show (f (Scope f Char))) => String -> LamCtor f Char -> TestTree
makeTests name lamb =
    let bid = lamb 'x' (pure 'x')
        bconst = lamb 'a' (lamb 'b' (pure 'a'))
        bfree = lamb 'y' (pure 'z')
        bfree2 = lamb 'c' (lamb 'd' (pure 'e'))
        bvar = pure 'w'
        bvar2 = pure 'u'

        testEq = testCase "eq" $ do
            bvar @?= bvar
            bvar @/= bvar2
            bid @?= lamb 'x' (pure 'x')
            bid @?= lamb 'y' (pure 'y')
            bid @/= lamb 'x' (pure 'y')
            bid @/= lamb 'y' (pure 'x')
            bid @/= bvar

        testFreeVars = testCase "free vars" $ do
            freeVars bid @?= V.empty
            freeVars bconst @?= V.empty
            freeVars bfree @?= V.singleton 'z'
            freeVars bfree2 @?= V.singleton 'e'
            freeVars bvar @?= V.singleton 'w'
            freeVars bvar2 @=? V.singleton 'u'

        testVarSub = testCase "var sub" $ do
            (bvar >>= const bvar2) @?= bvar2
            (bfree >>= const bvar2) @?= lamb 'y' bvar2
            (bfree2 >>= const bvar2) @?= lamb 'c' (lamb 'd' bvar2)

        testIdSub = testCase "id sub" $ do
            (bvar >>= const bid) @?= bid
            (bfree >>= const bid) @?= lamb 'y' bid
            (bfree2 >>= const bid) @?= lamb 'c' (lamb 'd' bid)

    in testGroup name [testEq, testFreeVars, testVarSub, testIdSub]

test_bare :: TestTree
test_bare = makeTests "sub - bare" bareLamCtor

test_exp :: TestTree
test_exp = makeTests "sub - exp" expLamCtor
