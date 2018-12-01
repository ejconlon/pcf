module Pcf.Printer (printExp, repExp) where

import           Bound.Name        (Name (..))
import           Data.Text         (Text)
import qualified Data.Text         as Text
import           Pcf.Functions     (instantiateAndThen')
import           Pcf.Types         (Exp (..), SExp (..), Ty (..))

printExp :: Exp Text -> Text
printExp = emit . repExp

emit :: SExp Text -> Text
emit (SAtom t)  = t
emit (SList ts) = "(" <> Text.intercalate " " (fmap emit ts) <> ")"

repTy :: Ty -> SExp Text
repTy Nat       = SAtom "Nat"
repTy (Arr l r) = SList [SAtom "->", repTy l, repTy r]

unassoc :: [SExp Text] -> Exp Text -> SExp Text
unassoc ts (App l r) = unassoc (repExp r : ts) l
unassoc ts x = SList (repExp x : ts)

repExp :: Exp Text -> SExp Text
repExp x = case x of
    Var a -> SAtom a
    App l r -> unassoc [repExp r] l
    Ifz g t e ->
        let g' = repExp g
            t' = repExp t
            e' = repExp e
        in SList [SAtom "ifz", g', t', e']
    Lam (Name n _) ty s ->
        let s' = instantiateAndThen' n s repExp
        in SList [SAtom "lam", SAtom n, repTy ty, s']
    Fix (Name n _) ty s ->
        let s' = instantiateAndThen' n s repExp
        in SList [SAtom "fix", SAtom n, repTy ty, s']
    Suc y ->
        let y' = repExp y
        in SList [SAtom "suc", y']
    Zero -> SAtom "zero"
