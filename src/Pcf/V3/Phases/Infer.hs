module Pcf.V3.Phases.Infer where

import           Bound                 (Scope)
import           Control.Lens          (modifying, view)
import           Control.Monad.Except  (MonadError (throwError))
import           Control.Monad.Reader  (MonadReader)
import           Control.Monad.State   (MonadState)
import           Data.Foldable         (traverse_)
import           Data.Generics.Product (field)
import           Data.Map              (Map)
import qualified Data.Map              as M
import           Data.Sequence         (Seq (..))
import qualified Data.Sequence         as Seq
import           GHC.Generics          (Generic)
import           Pcf.Core.BoundCrazy   (Instantiable (..), Sub, handle, instantiateE')
import           Pcf.Core.Func
import           Pcf.Core.Util         (modifyingM)
import           Pcf.V3.Names
import           Pcf.V3.Types

newtype U = U { unU :: Int } deriving (Generic, Show, Eq, Ord, Enum)

data InferType =
      UType U
    | ExpType Type0
    deriving (Generic, Eq, Show)

data InferEnv = InferEnv
    { ieDataDefs :: DataDefs0
    , ieTyMap    :: Map Name InferType
    } deriving (Generic, Show, Eq)

data InferError =
      InferUnknownTypeError Name
    | InferUnknownConError Name
    | InferUnboundVarError Int
    deriving (Generic, Show, Eq)

data Style = ExpStyle | PatStyle deriving (Generic, Show, Eq)

data Constraint =
      CTy U Type0
    | CEq U U
    | CCont U U
    | CCon Style U Name (Seq U)
    | CFun U (Seq U) U
    | CRet U U
    | CFree U Name
    deriving (Generic, Show, Eq)

data InferState = InferState
    { isConstraints :: Seq Constraint
    , isNext        :: U
    } deriving (Generic, Show, Eq)

emptyInferState :: InferState
emptyInferState = InferState Seq.empty (U 0)

type InferC m = (Instantiable m, MonadReader InferEnv m, MonadState InferState m, MonadError InferError m)
newtype InferT m a = InferT { unInferT :: FuncT InferEnv InferState InferError m a }
    deriving (Functor, Applicative, Monad, MonadReader InferEnv, MonadState InferState, MonadError InferError)

instance Monad m => Instantiable (InferT m) where
    instantiating = instantiateE' InferUnboundVarError

exploding :: (Instantiable m, MonadReader InferEnv m) => Seq (Sub Int Name InferType) -> Scope Int Exp0 Name -> (Exp0 Name -> m a) -> m a
exploding = handle (field @"ieTyMap")

inferProof :: Monad m => (forall n. InferC n => n a) -> InferT m a
inferProof = id

draw :: MonadState InferState m => m U
draw = modifyingM (field @"isNext") (pure . succ)

addC :: MonadState InferState m => Constraint -> m ()
addC = modifying (field @"isConstraints") . flip (:|>)

addInferType :: MonadState InferState m => U -> InferType -> m ()
addInferType u it =
    case it of
        UType v   -> addC (CEq u v)
        ExpType t -> addC (CTy u t)

addPats :: MonadState InferState m => U -> U -> Seq (U, U) -> m ()
addPats u v puvs = traverse_ (\(pu, pv) -> addC (CEq u pu) >> addC (CEq v pv)) puvs

buildVarAs :: InferC m => U -> Name -> m ()
buildVarAs u n = do
    tyMap <- view (field @"ieTyMap")
    case M.lookup n tyMap of
        Just it -> addInferType u it
        Nothing -> addC (CFree u n)

buildPat :: InferC m => Pat0 Name -> m (U, U)
buildPat p = do
    u <- draw
    v <- buildPatAs u p
    pure (u, v)

buildPatAs :: InferC m => U -> Pat0 Name -> m U
buildPatAs u p =
    case p of
        VarPat0 i b -> exploding (projectSub (i, UType u)) b build
        ConPat0 n is b -> do
            ivs <- traverse (const draw) is
            addC (CCon PatStyle u n ivs)
            exploding (projectSubs (Seq.zip is (UType <$> ivs))) b build

build :: InferC m => Exp0 Name -> m U
build e = do
    u <- draw
    buildAs u e
    pure u

buildAs :: InferC m => U -> Exp0 Name -> m ()
buildAs u e = do
    case e of
        Var0 a -> buildVarAs u a
        Let0 i el b -> do
            v <- build el
            exploding (projectSub (i, UType v)) b (buildAs u)
        Con0 n xs -> do
            xvs <- traverse build xs
            addC (CCon ExpStyle u n xvs)
        Case0 et ps -> do
            v <- build et
            puvs <- traverse buildPat ps
            addPats u v puvs
        Call0 el xs -> do
            v <- build el
            xvs <- traverse build xs
            addC (CFun v xvs u)
        Lam0 its b -> do
            -- not explicitly typed: ignore ty
            ivs <- traverse (const draw) its
            rv <- draw
            addC (CFun u ivs rv)
            exploding (projectSubs (Seq.zip (fst <$> its) (UType <$> ivs))) b (buildAs rv)
        CallCC0 i _ b -> do
            -- TODO suspicious impl
            -- not explicitly typed: ignore ty
            v <- draw
            addC (CCont u v)
            exploding (projectSub (i, UType v)) b (buildAs u)
        Throw0 c e -> do
            -- TODO suspicious impl
            pure ()
            -- v <- build c
            -- w <- build e
            -- addC (CEq u c)
        The0 et t -> do
            addC (CTy u t)
            buildAs u et

solve :: InferC m => m (Map U Type0)
solve = undefined
