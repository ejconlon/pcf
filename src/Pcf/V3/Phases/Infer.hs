module Pcf.V3.Phases.Infer where

import           Bound                 (Scope)
import           Control.Lens          (modifying, use, view)
import           Control.Monad         (unless)
import           Control.Monad.Except  (MonadError (throwError))
import           Control.Monad.Reader  (MonadReader)
import           Control.Monad.State   (MonadState)
import           Data.Foldable         (traverse_)
import           Data.Functor          (($>))
import           Data.Generics.Product (field)
import           Data.Map              (Map)
import qualified Data.Map              as M
import           Data.Maybe            (fromMaybe)
import           Data.Sequence         (Seq (..))
import qualified Data.Sequence         as Seq
import           Data.Void             (Void)
import           GHC.Generics          (Generic)
import           Pcf.Core.BoundCrazy   (Instantiable (..), Sub, handle, instantiateE')
import           Pcf.Core.Func
import           Pcf.Core.UnionFind    (buildEqs)
import           Pcf.Core.Util         (modifyingM, modifyingM_)
import           Pcf.V3.Names
import           Pcf.V3.Types

newtype U = U { unU :: Int } deriving (Generic, Show, Eq, Ord, Enum)

data GenEnv = GenEnv
    { geTyMap :: Map Name U
    } deriving (Generic, Show, Eq)

data GenError =
      GenUnboundVarError Int
    deriving (Generic, Show, Eq)

data KRHS =
      KVar Name
    | KEq U
    | KTy Type0
    | KCont U
    | KCon Name (Seq U)
    | KFun (Seq U) U
    deriving (Generic, Show, Eq)

data Konstraint = Konstraint U KRHS
    deriving (Generic, Show, Eq)

data GenState = GenState
    { gsKonstraints :: Seq Konstraint
    , gsNext        :: U
    } deriving (Generic, Show, Eq)

emptyGenState :: GenState
emptyGenState = GenState Seq.empty (U 0)

type GenC m = (Instantiable m, MonadReader GenEnv m, MonadState GenState m, MonadError GenError m)
newtype GenT m a = GenT { unGenT :: FuncT GenEnv GenState GenError m a }
    deriving (Functor, Applicative, Monad, MonadReader GenEnv, MonadState GenState, MonadError GenError)

instance Monad m => Instantiable (GenT m) where
    instantiating = instantiateE' GenUnboundVarError

exploding :: (Instantiable m, MonadReader GenEnv m) => Seq (Sub Int Name U) -> Scope Int Exp0 Name -> (Exp0 Name -> m a) -> m a
exploding = handle (field @"geTyMap")

genProof :: Monad m => (forall n. GenC n => n a) -> GenT m a
genProof = id

draw :: MonadState GenState m => m U
draw = modifyingM (field @"gsNext") (pure . succ)

addK :: MonadState GenState m => U -> KRHS -> m ()
addK u rhs = modifying (field @"gsKonstraints") (flip (:|>) (Konstraint u rhs))

buildPatAs :: GenC m => U -> U -> Pat0 Name -> m ()
buildPatAs v u p =
    case p of
        VarPat0 i b -> exploding (projectSub (i, v)) b (buildAs u)
        ConPat0 n is b -> do
            ivs <- traverse (const draw) is
            addK v (KCon n ivs)
            exploding (projectSubs (Seq.zip is ivs)) b (buildAs u)

build :: GenC m => Exp0 Name -> m U
build e = do
    u <- draw
    buildAs u e
    pure u

buildAs :: GenC m => U -> Exp0 Name -> m ()
buildAs u e = do
    case e of
        Var0 a -> do
            tyMap <- view (field @"geTyMap")
            case M.lookup a tyMap of
                Nothing -> addK u (KVar a)
                Just v -> addK u (KEq v)
        Let0 i el b -> do
            v <- build el
            exploding (projectSub (i, v)) b (buildAs u)
        Con0 n xs -> do
            xvs <- traverse build xs
            addK u (KCon n xvs)
        Case0 et ps -> do
            v <- build et
            traverse_ (buildPatAs v u) ps
        Call0 el xs -> do
            v <- build el
            xvs <- traverse build xs
            addK v (KFun xvs u)
        Lam0 its b -> do
            -- not explicitly typed: ignore ty
            ivs <- traverse (const draw) its
            rv <- draw
            addK u (KFun ivs rv)
            exploding (projectSubs (Seq.zip (fst <$> its) ivs)) b (buildAs rv)
        CallCC0 i _ b -> do
            -- not explicitly typed: ignore ty
            v <- draw
            addK v (KCont u)
            exploding (projectSub (i, v)) b (buildAs u)
        Throw0 c e -> do
            -- u can be anything, so no constraint on it!
            v <- build c
            w <- build e
            addK v (KCont w)
        The0 et t -> do
            addK u (KTy t)
            buildAs u et

isKEq :: Konstraint -> Bool
isKEq (Konstraint _ (KEq _)) = True
isKEq _ = False

projectKEq :: Konstraint -> (U, U)
projectKEq (Konstraint u (KEq v)) = (u, v)
projectKEq _ = error "should be filtered first"

gen :: GenC m => Exp0 Name -> m (Seq Konstraint, Map U U)
gen e = do
    build e
    ks <- use (field @"gsKonstraints")
    let (eqKs, nonEqKs) = Seq.partition isKEq ks
        eqs = buildEqs (projectKEq <$> eqKs)
    pure (nonEqKs, eqs)

data SolveEnv = SolveEnv
    { seDataDefs    :: DataDefs0
    , seTyMap       :: Map Name Type0
    , seKonstraints :: Seq Konstraint
    } deriving (Generic, Eq, Show)

data SolveState = SolveState
    { ssSolution :: Map U Type0
    , ssEqs :: Map U U
    } deriving (Generic, Eq, Show)

data SolveError =
      SolveFreeVarError U Name
    | SolveConflictingTypeError U Type0 Type0
    | SolveUnknownConError U Name
    | SolveConArityError U Name Int Int
    deriving (Generic, Eq, Show)

type SolveC m = (MonadReader SolveEnv m, MonadState SolveState m, MonadError SolveError m)
newtype SolveT m a = SolveT { unSolveT :: FuncT SolveEnv SolveState SolveError m a }
    deriving (Functor, Applicative, Monad, MonadReader SolveEnv, MonadState SolveState, MonadError SolveError)

solveProof :: Monad m => (forall n. SolveC n => n a) -> SolveT m a
solveProof = id

canonicalize :: MonadState SolveState m => U -> m U
canonicalize u = do
    eqs <- use (field @"ssEqs")
    pure (fromMaybe u (M.lookup u eqs))

addTySol :: SolveC m => U -> Type0 -> m ()
addTySol u ty0 = do
    modifyingM_ (field @"ssSolution") $ \m -> do
        case M.lookup u m of
            Just ty1 -> unless (ty0 == ty1) (throwError (SolveConflictingTypeError u ty0 ty1)) $> m
            Nothing -> pure (M.insert u ty0 m)

findTySol :: MonadState SolveState m => U -> m (Maybe Type0)
findTySol u = do
    sols <- use (field @"ssSolution")
    pure (M.lookup u sols)

findTyInfo :: MonadReader SolveEnv m => Name -> m (Maybe (Name, Seq Type0))
findTyInfo n = do
    dds <- view (field @"seDataDefs")
    case M.lookup n (conNameToTyNameAndDef dds) of
        Just (n, cd) -> pure (Just (n, conDefTypes cd))
        Nothing -> pure Nothing

handleC :: SolveC m => Konstraint -> m ()
handleC k@(Konstraint u0 rhs) = do
    u <- canonicalize u0
    case rhs of
        KVar n -> do
            tyMap <- view (field @"seTyMap")
            case M.lookup n tyMap of
                Just ty -> addTySol u ty
                Nothing -> throwError (SolveFreeVarError u n)
        KEq v0 -> error "TODO remove eqs"
        KTy ty -> addTySol u ty
        KCont v0 -> do
            v <- canonicalize v0
            ms <- findTySol v
            case ms of
                Just ty -> addTySol u (TyCont0 ty)
                Nothing -> error "TODO fix cont"
        KCon n vs0 -> do
            vs <- traverse canonicalize vs0
            mti <- findTyInfo n
            case mti of
                Just (tn, ts) -> do
                    let xlen = Seq.length vs
                        alen = Seq.length ts
                    unless (xlen == alen) (throwError (SolveConArityError u n xlen alen))
                    traverse_ (uncurry addTySol) (Seq.zip vs ts)
                    addTySol u (TyCon0 tn)
                Nothing -> throwError (SolveUnknownConError u n)
        KFun vs0 r0 -> do
            vs <- traverse canonicalize vs0
            r <- canonicalize r0
            mts <- traverse findTySol vs
            mrt <- findTySol r
            case (,) <$> sequence mts <*> mrt of
                Just (vts, rt) -> addTySol u (TyFun0 vts rt)
                Nothing -> error "TODO fix fun"

solve :: SolveC m => m ()
solve = do
    konstraints <- view (field @"seKonstraints")
    traverse_ handleC (Seq.reverse konstraints)
