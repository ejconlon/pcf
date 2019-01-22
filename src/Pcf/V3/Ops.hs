{-# LANGUAGE Rank2Types #-}

module Pcf.V3.Ops where

import           Control.Lens               (assign, use)
import           Control.Monad              (unless, when)
import           Control.Monad.Except       (ExceptT, MonadError (..), runExceptT)
import           Control.Monad.Identity     (Identity (..))
import           Control.Monad.Reader       (runReaderT)
import           Control.Monad.State        (MonadState (..), StateT, gets, runStateT)
import           Control.Monad.Trans        (MonadTrans (..))
import           Data.Foldable              (toList)
import           Data.Generics.Product      (field)
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Sequence              (Seq)
import qualified Data.Sequence              as Seq
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import           Data.Text                  (Text)
import           Data.Void                  (absurd)
import           GHC.Generics               (Generic)
import           Pcf.Core.Func
import           Pcf.Core.SExp              (SExp)
import           Pcf.Core.SExp.Parser       (Anno, readSExpAnno)
import           Pcf.Core.Util              (modifyingM)
import           Pcf.V3.Functions
import           Pcf.V3.Parser              (readExpX, readStmtX)
import           Pcf.V3.Types

data OpsData = OpsData
    { decls :: Map Name Type0
    , defns :: Map Name (Exp0 Name)
    , dataDefs :: DataDefs0
    } deriving (Generic, Eq, Show)

emptyOpsData :: OpsData
emptyOpsData = OpsData M.empty M.empty emptyDataDefs

-- TODO push some of these exceptions down into Functions
data OpsExc =
      AlreadyDeclared Name
    | NotDeclared Name
    | AlreadyDefined Name
    | DataAlreadyDeclared Name
    | ConAlreadyDeclared Name Name Name
    | CannotParseExp (SExp Anno Text)
    | CannotParseStmt (SExp Anno Text)
    | CannotParseSExp Text
    | WrapTypeError FullTypeError
    -- | WrapEvalError EvalError
    deriving (Generic, Eq, Show)

type OpsC m = (MonadState OpsData m, MonadError OpsExc m)

newtype OpsT m a = OpsT { unOps :: ExceptT OpsExc (StateT OpsData m) a }
    deriving (Functor, Applicative, Monad, MonadState OpsData, MonadError OpsExc)

type Ops a = OpsT Identity a

opsProof :: Monad m => (forall n. OpsC n => n a) -> OpsT m a
opsProof = id

instance MonadTrans OpsT where
    lift = OpsT . lift . lift

runOpsT :: OpsT m a -> OpsData -> m (Either OpsExc a, OpsData)
runOpsT ops = runStateT (runExceptT (unOps ops))

runOps :: Ops a -> OpsData -> (Either OpsExc a, OpsData)
runOps ops = runIdentity . runOpsT ops

liftFuncT :: Monad m => r -> s -> (e -> OpsExc) -> (FuncT r s e m a) -> OpsT m (a, s)
liftFuncT env st wrap act = do
    mr <- lift (runFuncT act env st)
    either (throwError . wrap) pure mr

liftTypeT :: Monad m => Map Name Type0 -> DataDefs0 -> TypeT m b -> OpsT m b
liftTypeT tyMap dds = (fst <$>) . liftFuncT (TypeEnv tyMap dds Seq.empty) () WrapTypeError

-- liftEvalT :: Monad m => Map Text (Exp0 Text) -> EvalT m b -> OpsT m b
-- liftEvalT expMap = (fst <$>) . liftFuncT () (EvalState KontTop0 Seq.empty (ExpTerm <$> expMap)) WrapEvalError

-- liftConvT :: Monad m => ConvT n n m b -> OpsT m b
-- liftConvT = (fst <$>) . liftFuncT (ConvEnv pure) () WrapConvError

-- liftLamLiftT :: Monad m => LamLiftT Text m b -> OpsT m b
-- liftLamLiftT = (fst <$>) . liftFuncT (LamLiftEnv pure) () WrapLamLiftError

-- liftFauxT :: Monad m => Monad m => FauxState Text -> FauxT Text m b -> OpsT m (b, FauxState Text)
-- liftFauxT st = liftFuncT () st WrapFauxError

parseSExp :: Monad m => Text -> OpsT m (SExp Anno Text)
parseSExp input = maybe (throwError (CannotParseSExp input)) pure (readSExpAnno input)

parseExp :: Monad m => SExp Anno Text -> OpsT m ExpX
parseExp se = maybe (throwError (CannotParseExp se)) pure (readExpX se)

parseStmt :: Monad m => SExp Anno Text -> OpsT m StmtX
parseStmt se = maybe (throwError (CannotParseStmt se)) pure (readStmtX se)

declare :: Monad m => Name -> Type0 -> OpsT m ()
declare name ty = modifyingM (field @"decls") $ \decls -> do
    when (M.member name decls) (throwError (AlreadyDeclared name))
    pure (M.insert name ty decls)

define :: Monad m => Name -> Exp0 Name -> OpsT m ()
define name e = do
    decls <- use (field @"decls")
    case M.lookup name decls of
        Nothing -> throwError (NotDeclared name)
        Just expectedTy -> modifyingM (field @"defns") $ \defns -> do
            when (M.member name defns) (throwError (AlreadyDefined name))
            dds <- use (field @"dataDefs")
            actualTy <- liftTypeT (M.delete name decls) dds (checkType0 expectedTy e)
            pure (M.insert name e defns)

dataDef :: Monad m => Name -> Seq ConDef0 -> OpsT m ()
dataDef name cds = modifyingM (field @"dataDefs") $ \dds -> do
    when (declaredDataType name dds) (throwError (DataAlreadyDeclared name))
    -- TODO iterate through cons and check for previous decls
    pure (addDataDef name cds dds)

processStmt :: Monad m => Stmt0 -> OpsT m ()
processStmt (Decl name ty)  = declare name ty
processStmt (Defn name exp) = define name exp
processStmt (Data name cds) = dataDef name cds

-- typeCheckOps :: Monad m => Exp0 Text -> OpsT m Type0
-- typeCheckOps e = do
--     decls <- use (field @"decls")
--     liftTypeT decls (inferType0 e)

-- bigStepOps :: Monad m => Exp0 Text -> OpsT m (Seq (Exp0 Name, EvalState), Maybe EvalError)
-- bigStepOps e = do
--     defns <- use (field @"defns")
--     liftEvalT defns (bigStep0 e)

freeVarsOps :: (Monad m, Ord a) => Exp0 a -> OpsT m (Set a)
freeVarsOps = pure . S.fromList . toList

-- closConvOps :: (Monad m, Eq n) => Exp n n -> OpsT m (ExpC n n)
-- closConvOps = liftConvT . closConv

-- lambdaLiftOps :: Monad m => ExpC Text -> OpsT m (ExpL Text)
-- lambdaLiftOps = liftLamLiftT . lambdaLift

-- fauxOps :: Monad m => FauxState Text -> ExpL Text -> OpsT m (ExpFC Text, FauxState Text)
-- fauxOps st = liftFauxT st . faux
