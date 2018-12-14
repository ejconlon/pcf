module Test.Pcf.SubTest where

import           Pcf.Sub
import           Test.Pcf.Assertions ((@/=))
import           Test.Tasty
import           Test.Tasty.HUnit

-- Exp

data ExpF n a =
      App a a
    | Ifz a a a
    | Zero
    | Suc a
    | Lam (NameOnly n) a
    deriving (Eq, Show, Functor, Foldable, Traversable)

type Exp n a = Scope (ExpF n) a

-- Smart constructors for Exp

var :: a -> Exp n a
var = pure

lam' :: Eq a => n -> a -> Exp n a -> Exp n a
lam' n a = wrapScope . Lam (Name n ()) . abstract1 a

lam :: Eq n => n -> Exp n n -> Exp n n
lam n = lam' n n

app :: Exp n a -> Exp n a -> Exp n a
app l r = wrapScope (App l r)

ifz :: Exp n a -> Exp n a -> Exp n a -> Exp n a
ifz g t e = wrapScope (Ifz g t e)

zero :: Exp n a
zero = wrapScope Zero

suc :: Exp n a -> Exp n a
suc = wrapScope  . Suc

-- Example terms

eid, econst, efree, efree2, evar, ezero :: Exp Char Char
eid = lam 'x' (var 'x')
econst = lam 'a' (lam 'b' (var 'a'))
efree = lam 'y' (var 'z')
efree2 = lam 'c' (lam 'd' (var 'e'))
evar = var 'w'
evar2 = var 'u'
ezero = zero

test_varsub :: TestTree
test_varsub = testCase "varsub" $ do
    (ezero >>= (const evar2)) @?= ezero
    (evar >>= (const evar2)) @?= evar2
    (efree >>= (const evar2)) @?= lam 'y' evar2
    (efree2 >>= (const evar2)) @?= lam 'c' (lam 'd' evar2)

test_idsub :: TestTree
test_idsub = testCase "idsub" $ do
    (ezero >>= (const eid)) @?= ezero
    (evar >>= (const eid)) @?= eid
    (efree >>= (const eid)) @?= lam 'y' eid
    -- (efree2 >>= (const eid)) @?= lam 'c' (lam 'd' eid)
