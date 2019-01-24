module Pcf.V3.Prelude where

import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Pcf.V3.Types

prelude :: Seq Stmt0
prelude = Seq.fromList
    [ Data "Void" Seq.Empty
    , Data "Unit" (Seq.singleton (ConDef "Unit" Seq.Empty))
    , Data "Bool" (Seq.fromList [ConDef "True" Seq.Empty, ConDef "False" Seq.Empty])
    , Data "Nat" (Seq.fromList [ConDef "Zero" Seq.Empty, ConDef "Succ" (Seq.singleton (TyCon0 "Nat"))])
    ]
