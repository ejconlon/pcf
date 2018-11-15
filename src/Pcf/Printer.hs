module Pcf.Printer (printExp) where

import Bound.Name (name)
import Control.Monad.Gen (MonadGen(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Pcf.Functions (instantiateAndThen)
import Pcf.Types (Exp(..), Ty(..))

printExp :: (MonadGen a m, Ord a) => (a -> Text) -> Exp a -> m Text
printExp f e = emit f <$> repExp Map.empty e

data Rep a = RepOne Text | RepMany [Rep a] | RepVar a deriving (Show, Eq)

emit :: (a -> Text) -> Rep a -> Text
emit showVar (RepVar a) = showVar a
emit _ (RepOne t) = t
emit f (RepMany ts) = "(" <> Text.intercalate " " (fmap (emit f) ts) <> ")"

repTy :: Ty -> Rep a
repTy Nat = RepOne "Nat"
repTy (Arr l r) = RepMany [repTy l, repTy r]

repExp :: (MonadGen a m, Ord a) => Map a Text -> Exp a -> m (Rep a)
repExp env x = case x of
    Var a -> pure (RepVar a)
    App l r -> do
        l' <- repExp env l
        r' <- repExp env r
        pure (RepMany [l', r'])
    Ifz g t e -> do
        g' <- repExp env g
        t' <- repExp env t
        e' <- repExp env e
        pure (RepMany [RepOne "ifz", g', t', e'])
    Lam i ty s -> do
        s' <- instantiateAndThen (name i) env s repExp
        pure (RepMany [RepOne "lam", RepOne (name i), repTy ty, s'])
    Fix i ty s -> do
        s' <- instantiateAndThen (name i) env s repExp
        pure (RepMany [RepOne "fix", RepOne (name i), repTy ty, s'])
    Suc y -> do
        y' <- repExp env y
        pure (RepMany [RepOne "suc", y'])
    Zero -> pure (RepOne "zero")
