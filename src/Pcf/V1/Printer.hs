module Pcf.V1.Printer (emit, repTy, printTy, repExp, printExp, repStmt, printStmt) where

import           Bound.Name       (Name (..))
import           Data.Text        (Text)
import           Data.Vector      (Vector)
import qualified Data.Vector      as V
import           Pcf.Core.BoundUtil (instantiateAndThen')
import           Pcf.Core.SExp    (SExp (..))
import           Pcf.Core.SExp.Printer (emit)
import           Pcf.V1.Types     (Exp (..), Stmt (..), Ty (..))

unassoc :: Vector (SExp () Text) -> Exp Text -> SExp () Text
unassoc ts (App l r) = unassoc (V.cons (repExp r) ts) l
unassoc ts x         = SList () (V.cons (repExp x) ts)

repTy :: Ty -> SExp () Text
repTy Nat       = SAtom () "Nat"
repTy (Arr l r) = SList () (V.fromList [SAtom () "->", repTy l, repTy r])

printTy :: Ty -> Text
printTy = emit . repTy

repExp :: Exp Text -> SExp () Text
repExp x = case x of
    Var a -> SAtom () a
    App l r -> unassoc (V.singleton (repExp r)) l
    Ifz g t e ->
        let g' = repExp g
            t' = repExp t
            e' = repExp e
        in SList () (V.fromList [SAtom () "ifz", g', t', e'])
    Lam (Name n _) ty s ->
        let s' = instantiateAndThen' n s repExp
        in SList () (V.fromList [SAtom () "lam", SAtom () n, repTy ty, s'])
    Fix (Name n _) ty s ->
        let s' = instantiateAndThen' n s repExp
        in SList () (V.fromList [SAtom () "fix", SAtom () n, repTy ty, s'])
    Suc y ->
        let y' = repExp y
        in SList () (V.fromList [SAtom () "suc", y'])
    Zero -> SAtom () "zero"

printExp :: Exp Text -> Text
printExp = emit . repExp

repStmt :: Stmt Text -> SExp () Text
repStmt x = case x of
    Decl n ty -> SList () (V.fromList [SAtom () "decl", SAtom () n, repTy ty])
    Defn n e  -> SList () (V.fromList [SAtom () "defn", SAtom () n, repExp e])

printStmt :: Stmt Text -> Text
printStmt = emit .repStmt
