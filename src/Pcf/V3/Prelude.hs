module Pcf.V3.Prelude where

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Pcf.V3.Types

prelude :: Seq Stmt0
prelude = Seq.fromList
    [ Data0 "Void" Seq.Empty
    , Data0 "Unit" (Seq.singleton (ConDef0 "Unit" Seq.Empty))
    , Data0 "Bool" (Seq.fromList [ConDef0 "True" Seq.Empty, ConDef0 "False" Seq.Empty])
    , Data0 "Nat" (Seq.fromList [ConDef0 "Zero" Seq.Empty, ConDef0 "Succ" (Seq.singleton (TyCon0 "Nat"))])
    ]
