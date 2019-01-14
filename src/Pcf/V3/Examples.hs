module Pcf.V3.Examples where

import qualified Data.Sequence    as Seq
import           Pcf.V3.Functions
import           Pcf.V3.Types

goOne0 :: Exp0 Name
goOne0 = lam0 (Seq.singleton ("a", TyBool0)) (Call0 (Var0 "xor") (Seq.singleton (Var0 "a")))

goAll0 :: Exp0 Name
goAll0 = lam0 (Seq.fromList [("a", TyBool0), ("b", TyBool0)]) (Call0 (Var0 "xor") (Seq.fromList [Var0 "a", Var0 "b"]))
