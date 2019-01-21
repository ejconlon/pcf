module Pcf.V3.Printer where

import           Data.Foldable         (toList)
import           Data.Sequence         (Seq (..))
import qualified Data.Sequence         as Seq
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Pcf.Core.SExp         (SExp (..))
import           Pcf.Core.SExp.Printer (emit)
import           Pcf.V3.Types


repTy0 :: Type0 -> SExp () Text
repTy0 (TyCon0 n)    = SAtom () n
repTy0 (TyFun0 xs r) = SList () (SAtom () "Fun" :<| SList () (repTy0 <$> xs) :<| repTy0 r :<| Seq.Empty)
repTy0 (TyCont0 t)   = SList () (SAtom () "Cont" :<| repTy0 t :<| Seq.Empty)

printTy0 :: Type0 -> Text
printTy0 = emit . repTy0

repExp0 :: Exp0 Text -> SExp () Text
repExp0 = undefined

printExp0 :: Exp0 Text -> Text
printExp0 = emit . repExp0

repConDef0 :: ConDef0 -> SExp () Text
repConDef0 = undefined

printConDef0 :: ConDef0 -> Text
printConDef0 = emit . repConDef0

repStmt0 :: Stmt0 -> SExp () Text
repStmt0 x = case x of
    Decl0 n ty -> SList () (SAtom () "decl" :<| SAtom () n :<| repTy0 ty :<| Seq.Empty)
    Defn0 n e  -> SList () (SAtom () "defn" :<| SAtom () n :<| repExp0 e :<| Seq.Empty)
    Data0 n cs -> SList () (SAtom () "data" :<| SAtom () n :<| end) where
        end = case cs of
            Seq.Empty -> Seq.Empty
            _ -> SList () (repConDef0 <$> cs) :<| Seq.Empty

printStmt0 :: Stmt0 -> Text
printStmt0 = emit . repStmt0
