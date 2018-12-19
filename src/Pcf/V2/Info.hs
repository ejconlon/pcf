module Pcf.V2.Info where

import           GHC.Generics    (Generic)
import qualified Text.Megaparsec as MP

data Anno = Anno { annoStart :: MP.SourcePos, annoEnd :: MP.SourcePos }
    deriving (Generic, Eq, Show)
