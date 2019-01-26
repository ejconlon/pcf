module Pcf.Core.BoundCrazy where

import Bound         (Scope (..), Var (..), abstract)
import Control.Monad.Except (MonadError (throwError))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)
import Pcf.Core.Util (findL, trabind)

type Sub b n a = (b, n, a)
type SubK b n = (b, n)
type SubV n v = (n, v)

subK :: Sub b n v -> SubK b n
subK (b, n, _) = (b, n)

subV :: Sub b n v -> SubV n v
subV (_, n, a) = (n, a)

instantiateM :: (Applicative m, Traversable f, Monad f) => (b -> m (f n)) -> Scope b f n -> m (f n)
instantiateM k (Scope e) = trabind e g where
    g v = case v of
        B b  -> k b
        F ea -> pure ea

instantiate1M :: (Applicative m, Traversable f, Monad f) => m (f n) -> Scope () f n -> m (f n)
instantiate1M = instantiateM . const

abstractM :: (Applicative m, Traversable f, Applicative f) => (n -> m (Maybe b)) -> f n -> m (Scope b f n)
abstractM f e = Scope <$> traverse g e where
    g a = h a <$> f a
    h a = maybe (F (pure a)) B

abstract1M :: (Applicative m, Traversable f, Applicative f, Eq n) => m n -> f n -> m (Scope () f n)
abstract1M ma = abstractM (\a1 -> (\a0 -> if a1 == a0 then Just () else Nothing) <$> ma)

absSubK :: Eq n => Seq (SubK b n) -> (n -> Maybe b)
absSubK sks y = findL (\(b, n) -> if y == n then Just b else Nothing) sks

instSubK :: Eq b => Seq (SubK b n) -> (b -> Maybe n)
instSubK sks x = findL (\(b, n) -> if x == b then Just n else Nothing) sks

looking :: (Applicative m, Eq b, Applicative f) => (b -> m (f n)) -> Seq (SubK b n) -> (b -> m (f n))
looking e sks x = maybe (e x) (pure . pure) (instSubK sks x)

instantiateE :: (Applicative m, Eq b, Traversable f, Monad f) => (b -> m (f n)) -> Seq (SubK b n) -> Scope b f n -> m (f n)
instantiateE e = instantiateM . looking e

instantiateE' :: (MonadError e m, Eq b, Traversable f, Monad f) => (b -> e) -> Seq (SubK b n) -> Scope b f n -> m (f n)
instantiateE' e = instantiateE (throwError . e)

abstracting :: (Monad f, Eq n) => Seq (SubK b n) -> f n -> Scope b f n
abstracting = abstract . absSubK

abstracting1 :: (Monad f, Eq n) => n -> f n -> Scope Int f n
abstracting1 n = abstracting (Seq.singleton (0, n))

-- TODO consider abstracting index type?
class Instantiable m where
    instantiating :: (Traversable f, Monad f) => Seq (SubK Int n) -> Scope Int f n -> m (f n)

newtype Inst a = Inst { unSimpleInst :: Either Int a }
    deriving (Generic, Show, Eq, Functor, Applicative, Monad, Foldable, Traversable, MonadError Int)

instance Instantiable Inst where
    instantiating = instantiateE (Inst . Left)
