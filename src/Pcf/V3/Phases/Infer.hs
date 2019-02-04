module Pcf.V3.Phases.Infer where

import           Bound                 (Scope)
import           Control.Lens          (assign, modifying, use, view)
import           Control.Monad         (unless)
import           Control.Monad.Except  (MonadError (throwError))
import           Control.Monad.Reader  (MonadReader)
import           Control.Monad.State   (MonadState)
import           Data.Foldable         (toList, traverse_)
import           Data.Functor          (($>))
import           Data.Generics.Product (field)
import           Data.Map              (Map)
import qualified Data.Map              as M
import           Data.Maybe            (fromMaybe)
import qualified Data.Set              as S
import           Data.Sequence         (Seq (..))
import qualified Data.Sequence         as Seq
import           Data.Void             (Void)
import           GHC.Generics          (Generic)
import           Pcf.Core.BoundCrazy   (Instantiable (..), Sub, abstracting, handle, instantiateE')
import           Pcf.Core.Func
import           Pcf.Core.OrdNub       (ordNub)
import           Pcf.Core.UnionFind
import           Pcf.Core.Util         (izip, modifyingM, modifyingM_, zooming)
import           Pcf.V3.Names
import           Pcf.V3.Types

newtype U = U { unU :: Int } deriving (Generic, Show, Eq, Ord, Enum)

schematize :: TypeI Name -> TySchemaI Name
schematize ti =
    let ns = ordNub (toList ti)
        is = ConcreteIdent <$> ns
        sks = izip ns
    in TySchemaI is (abstracting sks ti)

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

gen :: GenC m => Exp0 Name -> m (U, Seq Konstraint)
gen e = do
    u <- build e
    ks <- use (field @"gsKonstraints")
    pure (u, ks)

data IntegrateEnv = IntegrateEnv
    { ieDataDefs    :: DataDefs0
    , ieTyMap       :: Map Name Type0
    } deriving (Generic, Eq, Show)

data IntegrateState = IntegrateState
    { isSolution :: Map U (TypeI U)
    , isUnionFind :: UnionFindState U
    } deriving (Generic, Eq, Show)

emptyIntegrateState :: IntegrateState
emptyIntegrateState = IntegrateState M.empty emptyUnionFindState

data IntegrateError =
      IntegrateFreeVarError U Name
    | IntegrateUnknownConError U Name
    | IntegrateConArityError U Name Int Int
    deriving (Generic, Eq, Show)

type IntegrateC m = (MonadReader IntegrateEnv m, MonadState IntegrateState m, MonadError IntegrateError m)
newtype IntegrateT m a = IntegrateT { unIntegrateT :: FuncT IntegrateEnv IntegrateState IntegrateError m a }
    deriving (Functor, Applicative, Monad, MonadReader IntegrateEnv, MonadState IntegrateState, MonadError IntegrateError)

integrateProof :: Monad m => (forall n. IntegrateC n => n a) -> IntegrateT m a
integrateProof = id

canonicalize :: MonadState IntegrateState m => U -> m U
canonicalize = zooming (field @"isUnionFind") . lookupUF

addTyEq :: MonadState IntegrateState m => U -> U -> m ()
addTyEq u v = zooming (field @"isUnionFind") (linkUF u v)

unify :: IntegrateC m => TypeI U -> TypeI U -> m (TypeI U)
unify ty0 ty1 = error "TODO impl unify"
    -- case (ty0, ty1) of
    --     (TyVarI v0, _) -> pure ty1'
    --     (ty0', TyVarI v1) -> pure ty0'
    --     (TyConI n0, TyConI n1) -> if n0 == n1 then pure ty1 else error "TODO mismatch exc"

addTySol :: IntegrateC m => U -> TypeI U -> m ()
addTySol u ty0 =
    modifyingM_ (field @"isSolution") $ \m -> do
        ty' <- maybe (pure ty0) (unify ty0) (M.lookup u m)
        pure (M.insert u ty' m)

findTyInfo :: MonadReader IntegrateEnv m => Name -> m (Maybe (Name, Seq Type0))
findTyInfo n = do
    dds <- view (field @"ieDataDefs")
    case M.lookup n (conNameToTyNameAndDef dds) of
        Just (n, cd) -> pure (Just (n, conDefTypes cd))
        Nothing -> pure Nothing

-- TODO shouldn't have to canonicalize, that will be done by unify
handleC :: IntegrateC m => Konstraint -> m ()
handleC (Konstraint u0 rhs) = do
    u <- canonicalize u0
    case rhs of
        KVar n -> do
            tyMap <- view (field @"ieTyMap")
            case M.lookup n tyMap of
                Just ty -> addTySol u (liftTyI ty)
                Nothing -> throwError (IntegrateFreeVarError u n)
        KEq v0 -> do
            v <- canonicalize v0
            addTyEq u v
        KTy ty -> addTySol u (liftTyI ty)
        KCont v0 -> do
            v <- canonicalize v0
            addTySol u (TyContI (TyVarI v))
        KCon n vs0 -> do
            vs <- traverse canonicalize vs0
            mti <- findTyInfo n
            case mti of
                Just (tn, ts) -> do
                    let xlen = Seq.length vs
                        alen = Seq.length ts
                    unless (xlen == alen) (throwError (IntegrateConArityError u n xlen alen))
                    traverse_ (uncurry addTySol) (Seq.zip vs (liftTyI <$> ts))
                    addTySol u (TyConI tn)
                Nothing -> throwError (IntegrateUnknownConError u n)
        KFun vs0 r0 -> do
            vs <- traverse canonicalize vs0
            r <- canonicalize r0
            addTySol u (TyFunI (TyVarI <$> vs) (TyVarI r))

integrate :: IntegrateC m => Seq Konstraint -> m (Map U (TypeI U), UnionFindState U)
integrate ks = do
    traverse_ handleC ks
    sol <- use (field @"isSolution")
    ufs <- use (field @"isUnionFind")
    pure (sol, ufs)
