module Pcf.V1.Printer (emit, repTy, printTy, repExp, printExp, repStmt, printStmt) where

import           Bound.Name       (Name (..))
import           Data.Text        (Text)
import qualified Data.Text        as Text
import           Pcf.Core.BoundUtil (instantiateAndThen')
import           Pcf.V1.Types     (Exp (..), SExp (..), Stmt (..), Ty (..))

emit :: SExp Text -> Text
emit (SAtom t)  = t
emit (SList ts) = "(" <> Text.intercalate " " (fmap emit ts) <> ")"

unassoc :: [SExp Text] -> Exp Text -> SExp Text
unassoc ts (App l r) = unassoc (repExp r : ts) l
unassoc ts x         = SList (repExp x : ts)

repTy :: Ty -> SExp Text
repTy Nat       = SAtom "Nat"
repTy (Arr l r) = SList [SAtom "->", repTy l, repTy r]

printTy :: Ty -> Text
printTy = emit . repTy

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

printExp :: Exp Text -> Text
printExp = emit . repExp

repStmt :: Stmt Text -> SExp Text
repStmt x = case x of
    Decl n ty -> SList [SAtom "decl", SAtom n, repTy ty]
    Defn n e  -> SList [SAtom "defn", SAtom n, repExp e]

printStmt :: Stmt Text -> Text
printStmt = emit .repStmt
