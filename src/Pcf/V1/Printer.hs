module Pcf.V1.Printer (emit, repTy, printTy, repExp, printExp, repStmt, printStmt) where

import           Bound.Name            (Name (..))
import           Data.Sequence         (Seq (..))
import qualified Data.Sequence         as Seq
import           Data.Text             (Text)
import           Pcf.Core.BoundUtil    (instantiateAndThen')
import           Pcf.Core.SExp         (SExp (..))
import           Pcf.Core.SExp.Printer (emit)
import           Pcf.V1.Types          (Exp (..), Stmt (..), Ty (..))

unassoc :: Seq (SExp () Text) -> Exp Text -> SExp () Text
unassoc ts (App l r) = unassoc ((repExp r) :<| ts) l
unassoc ts x         = SList () ((repExp x) :<| ts)

repTy :: Ty -> SExp () Text
repTy Nat       = SAtom () "Nat"
repTy (Arr l r) = SList () (Seq.fromList [SAtom () "->", repTy l, repTy r])

printTy :: Ty -> Text
printTy = emit . repTy

repExp :: Exp Text -> SExp () Text
repExp x = case x of
    Var a -> SAtom () a
    App l r -> unassoc (Seq.singleton (repExp r)) l
    Ifz g t e ->
        let g' = repExp g
            t' = repExp t
            e' = repExp e
        in SList () (Seq.fromList [SAtom () "ifz", g', t', e'])
    Lam (Name n _) ty s ->
        let s' = instantiateAndThen' n s repExp
        in SList () (Seq.fromList [SAtom () "lam", SAtom () n, repTy ty, s'])
    Fix (Name n _) ty s ->
        let s' = instantiateAndThen' n s repExp
        in SList () (Seq.fromList [SAtom () "fix", SAtom () n, repTy ty, s'])
    Suc y ->
        let y' = repExp y
        in SList () (Seq.fromList [SAtom () "suc", y'])
    Zero -> SAtom () "zero"

printExp :: Exp Text -> Text
printExp = emit . repExp

repStmt :: Stmt Text -> SExp () Text
repStmt x = case x of
    Decl n ty -> SList () (Seq.fromList [SAtom () "decl", SAtom () n, repTy ty])
    Defn n e  -> SList () (Seq.fromList [SAtom () "defn", SAtom () n, repExp e])

printStmt :: Stmt Text -> Text
printStmt = emit .repStmt
