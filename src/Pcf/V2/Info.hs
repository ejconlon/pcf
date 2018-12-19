module Pcf.V2.Info where

import           GHC.Generics    (Generic)
import qualified Text.Megaparsec as MP

data Anno = Anno { annoStart :: MP.SourcePos, annoEnd :: MP.SourcePos }
    deriving (Generic, Eq, Show)

data Info i a = Info { infoData :: i, infoValue :: a }
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

instance Monoid i => Applicative (Info i) where
    pure = Info mempty
    Info i f <*> Info j a = Info (i <> j) (f a)

-- Compose base Data.Functor.Compose puts Eq1 constraints on Eq. No bueno.
newtype Compose f g a = Compose { unCompose :: f (g a) }
    deriving (Generic, Eq, Show, Functor, Foldable, Traversable)

type InfoF i f = Compose (Info i) f

type AnnoF f = InfoF Anno f
