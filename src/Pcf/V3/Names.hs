module Pcf.V3.Names where

import           Control.Applicative  (Alternative (empty))
import           Data.Sequence        (Seq (..))
import qualified Data.Sequence        as Seq
import           Data.String          (IsString)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Pcf.Core.BoundCrazy  (Sub, SubK, SubV)

newtype Name = Name { unName :: Text } deriving (Generic, Eq, Ord, Show, IsString)

data Ident = ConcreteIdent Name | WildIdent deriving (Generic, Eq, Show)

-- I hate everything after this line

foldIdent :: (Name -> a) -> a -> (Ident -> a)
foldIdent f z i =
    case i of
        ConcreteIdent i -> f i
        WildIdent -> z

selectName :: Alternative t => Ident -> t Name
selectName = foldIdent pure empty

selectNames :: (Monad t, Alternative t) => t Ident -> t Name
selectNames = (>>= selectName)

selectSubV :: Alternative t => SubV Ident v -> t (SubV Name v)
selectSubV (i, v) =  (, v) <$> selectName i

selectSubVs :: (Monad t, Alternative t) => t (SubV Ident v) -> t (SubV Name v)
selectSubVs = (>>= selectSubV)

selectSub :: Alternative t => Sub b Ident v -> t (Sub b Name v)
selectSub (b, i, v) = (b, , v) <$> selectName i

selectSubs :: (Monad t, Alternative t) => t (Sub b Ident v) -> t (Sub b Name v)
selectSubs = (>>= selectSub)

projectSubK :: Alternative t => Ident -> t (SubK Int Name)
projectSubK i = (0, ) <$> selectName i

projectSubKs :: Seq Ident -> Seq (SubK Int Name)
projectSubKs = go 0 where
    go c is =
        case is of
            x :<| xs ->
                let c' = c + 1
                in case x of
                    ConcreteIdent n -> (c, n) :<| go c' xs
                    WildIdent -> go c' xs
            Seq.Empty -> Seq.empty

projectSub :: Alternative t => SubV Ident v -> t (Sub Int Name v)
projectSub (i, v) = (0, , v) <$> selectName i

projectSubs :: Seq (SubV Ident v) -> Seq (Sub Int Name v)
projectSubs = go 0 where
    go c is =
        case is of
            (xi, xv) :<| xs ->
                let c' = c + 1
                in case xi of
                    ConcreteIdent n -> (c, n, xv) :<| go c' xs
                    WildIdent -> go c' xs
            Seq.Empty -> Seq.empty
