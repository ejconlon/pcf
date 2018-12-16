module Pcf.V2.Printer (emit, repTy, printTy, repExp, printExp, repStmt, printStmt) where

import           Data.Text        (Text)
import qualified Data.Text        as T
import           Pcf.Core.Sub
import           Pcf.V2.Types     (Exp (..), ExpF(..), ExpFold, ExpN (..), SExp (..), Stmt (..), Ty (..))

emit :: SExp Text -> Text
emit (SAtom t)  = t
emit (SList ts) = "(" <> T.intercalate " " (emit <$> ts) <> ")"

unassoc :: [SExp Text] -> Exp Text Text -> Maybe (SExp Text)
unassoc ts e =
    case matchFunctor e of
        Just (App l r) -> (\r' -> unassoc (r' : ts) l) =<< repExp r
        _ -> (\e' -> SList (e' : ts)) <$> repExp e

repTy :: Ty -> SExp Text
repTy Nat       = SAtom "Nat"
repTy (Arr l r) = SList [SAtom "->", repTy l, repTy r]

printTy :: Ty -> Text
printTy = emit . repTy

repExpSF :: ExpFold Text Text (Maybe (SExp Text))
repExpSF = ScopeFold bound free binder functor where
    bound = const Nothing
    free = pure . SAtom
    binder b = do
        let i = binderInfo b
            n = nameKey (expName i)
            ty = expTy i
        s <- apply1 (pure n) b
        s' <- repExp s
        pure (SList [SAtom "lam", SAtom n, repTy ty, s'])
    functor = \case
        App l r -> do
            r' <- repExp r
            unassoc [r'] l
        Ifz g t e -> do
            g' <- repExp g
            t' <- repExp t
            e' <- repExp e
            pure (SList [SAtom "ifz", g', t', e'])
        Suc y -> do
            y' <- repExp y
            pure (SList [SAtom "suc", y'])
        Zero -> pure (SAtom "zero")

repExp :: Exp Text Text -> Maybe (SExp Text)
repExp = foldScope repExpSF

printExp :: Exp Text Text -> Maybe Text
printExp = (emit <$>) . repExp

repStmt :: Stmt Text Text -> Maybe (SExp Text)
repStmt x = case x of
    Decl n ty -> pure (SList [SAtom "decl", SAtom n, repTy ty])
    Defn n e  -> (\e' -> SList [SAtom "defn", SAtom n, e']) <$> repExp e

printStmt :: Stmt Text Text -> Maybe Text
printStmt = (emit <$>) . repStmt
