module Pcf.Core.SExp.Printer where

import           Data.Foldable (toList)
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Pcf.Core.SExp (SExp (..))

emit :: SExp i Text -> Text
emit (SAtom _ t)  = t
emit (SList _ ts) = "(" <> T.intercalate " " (emit <$> toList ts) <> ")"
