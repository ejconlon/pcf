module Pcf.V2.Printer (repTy, printTy, repExp, printExp, repStmt, printStmt) where

import           Data.Foldable         (toList)
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Vector           (Vector)
import qualified Data.Vector           as V
import           Pcf.Core.SExp         (SExp (..))
import           Pcf.Core.SExp.Printer (emit)
import           Pcf.Core.Sub
import           Pcf.V2.Types          (Exp (..), ExpF (..), ExpN (..), Stmt (..), Ty (..))

unassoc :: (ThrowSub m, Monad m) => Vector (SExp () Text) -> Exp Text Text -> m (SExp () Text)
unassoc ts e =
    case matchFunctor e of
        Just (App l r) -> (\r' -> unassoc (V.cons r' ts) l) =<< repExp r
        _              -> (\e' -> SList () (V.cons e' ts)) <$> repExp e

repTy :: Ty -> SExp () Text
repTy Nat       = SAtom () "Nat"
repTy (Arr l r) = SList () (V.fromList [SAtom () "->", repTy l, repTy r])

printTy :: Ty -> Text
printTy = emit . repTy

repExp :: (ThrowSub m, Monad m) => Exp Text Text -> m (SExp () Text)
repExp = foldScope sf where
    sf = boundFold free binder functor

    free _ = pure . SAtom ()

    binder _ b = do
        let i = binderInfo b
            n = nameKey (expName i)
            ty = expTy i
        s <- apply1 (pure n) b
        s' <- repExp s
        pure (SList () (V.fromList [SAtom () "lam", SAtom () n, repTy ty, s']))

    functor _ = \case
        App l r -> do
            r' <- repExp r
            unassoc (V.singleton r') l
        Ifz g t e -> do
            g' <- repExp g
            t' <- repExp t
            e' <- repExp e
            pure (SList () (V.fromList [SAtom () "ifz", g', t', e']))
        Suc y -> do
            y' <- repExp y
            pure (SList () (V.fromList [SAtom () "suc", y']))
        Zero -> pure (SAtom () "zero")

printExp :: (ThrowSub m, Monad m) => Exp Text Text -> m Text
printExp = (emit <$>) . repExp

repStmt :: (ThrowSub m, Monad m) => Stmt Text Text -> m (SExp () Text)
repStmt x = case x of
    Decl n ty -> pure (SList () (V.fromList [SAtom () "decl", SAtom () n, repTy ty]))
    Defn n e  -> (\e' -> SList () (V.fromList [SAtom () "defn", SAtom () n, e'])) <$> repExp e

printStmt :: (ThrowSub m, Monad m) => Stmt Text Text -> m Text
printStmt = (emit <$>) . repStmt
