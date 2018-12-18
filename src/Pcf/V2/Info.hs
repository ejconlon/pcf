module Pcf.V2.Info where

import GHC.Generics (Generic)
import qualified Text.Megaparsec as MP

data Anno = Anno { annoStart :: !MP.SourcePos, annoEnd :: !MP.SourcePos }
    deriving (Generic, Eq, Show)

data Info i a = Info { infoData :: !i, infoValue :: !a }
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)
