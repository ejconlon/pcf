{-# LANGUAGE Rank2Types #-}

module Pcf.V3.Ops where

import           Control.Lens               (assign, use)
import           Control.Monad              (unless)

import           Control.Monad.Except       (ExceptT, MonadError (..), runExceptT)
import           Control.Monad.Reader       (runReaderT)
import           Control.Monad.State.Strict (MonadState (..), StateT, gets, runStateT)
import           Control.Monad.Trans        (MonadTrans (..))
import           Data.Foldable              (toList)
import           Data.Generics.Product      (field)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
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
import           Pcf.Core.Sub               (SubError, scopeFreeVars)
import           Pcf.V3.Functions
import           Pcf.V3.Parser              (readExp0, readStmt0)
import           Pcf.V3.Types

data OpsData = OpsData
    { decls :: Map Text Type0
    , defns :: Map Text (Exp0 Text)
    } deriving (Generic, Eq, Show)

emptyOpsData :: OpsData
emptyOpsData = OpsData M.empty M.empty

-- TODO push some of these exceptions down into Functions
data OpsExc =
      AlreadyDeclared Text
    | NotDeclared Text
    | AlreadyDefined Text
    | CannotParseExp (SExp Anno Text)
    | CannotParseStmt (SExp Anno Text)
    | CannotParseSExp Text
    | WrapTypeError (EnvError TypeEnv TypeError)
    | WrapEvalError EvalError
    deriving (Generic, Eq, Show)

type OpsC m = (MonadState OpsData m, MonadError OpsExc m)

newtype OpsT m a = OpsT { unOps :: ExceptT OpsExc (StateT OpsData m) a }
    deriving (Functor, Applicative, Monad, MonadState OpsData, MonadError OpsExc)

opsProof :: Monad m => (forall n. OpsC n => n a) -> OpsT m a
opsProof = id

instance MonadTrans OpsT where
    lift = OpsT . lift . lift

runOpsT :: OpsT m a -> OpsData -> m (Either OpsExc a, OpsData)
runOpsT ops = runStateT (runExceptT (unOps ops))

liftFuncT :: Monad m => r -> s -> (e -> OpsExc) -> (FuncT r s e m a) -> OpsT m (a, s)
liftFuncT env st wrap act = do
    mr <- lift (runFuncT act env st)
    either (throwError . wrap) pure mr

liftTypeT :: Monad m => Map Text Type0 -> TypeT m b -> OpsT m b
liftTypeT tyMap = (fst <$>) . liftFuncT (TypeEnv tyMap Seq.empty) () WrapTypeError

liftEvalT :: Monad m => Map Text (Exp0 Text) -> EvalT m b -> OpsT m b
liftEvalT expMap = (fst <$>) . liftFuncT (EvalEnv expMap) (EvalState KontTop0) WrapEvalError

-- liftConvT :: Monad m => ConvT n n m b -> OpsT m b
-- liftConvT = (fst <$>) . liftFuncT (ConvEnv pure) () WrapConvError

-- liftLamLiftT :: Monad m => LamLiftT Text m b -> OpsT m b
-- liftLamLiftT = (fst <$>) . liftFuncT (LamLiftEnv pure) () WrapLamLiftError

-- liftFauxT :: Monad m => Monad m => FauxState Text -> FauxT Text m b -> OpsT m (b, FauxState Text)
-- liftFauxT st = liftFuncT () st WrapFauxError

parseSExp :: Monad m => Text -> OpsT m (SExp Anno Text)
parseSExp input = maybe (throwError (CannotParseSExp input)) pure (readSExpAnno input)

parseExp :: Monad m => SExp Anno Text -> OpsT m (Exp0 Text)
parseExp se = maybe (throwError (CannotParseExp se)) pure (readExp0 se)

parseStmt :: Monad m => SExp Anno Text -> OpsT m (Stmt0 Text)
parseStmt se = maybe (throwError (CannotParseStmt se)) pure (readStmt0 se)

declare :: Monad m => Text -> Type0 -> OpsT m ()
declare name ty = do
    decls <- use (field @"decls")
    if M.member name decls
        then throwError (AlreadyDeclared name)
        else pure ()
    let decls' = M.insert name ty decls
    assign (field @"decls") decls'
    pure ()

define :: Monad m => Text -> Exp0 Text -> OpsT m ()
define name e = do
    decls <- use (field @"decls")
    case M.lookup name decls of
        Nothing -> throwError (NotDeclared name)
        Just expectedTy -> do
            defns <- use (field @"defns")
            if M.member name defns
                then throwError (AlreadyDefined name)
                else pure ()
            actualTy <- liftTypeT (M.delete name decls) (checkType0 expectedTy e)
            let defns' = M.insert name e defns
            assign (field @"defns") defns'

processStmt :: Monad m => Stmt0 Text -> OpsT m ()
processStmt (Decl0 name ty)  = declare name ty
processStmt (Defn0 name exp) = define name exp

typeCheckOps :: Monad m => Exp0 Text -> OpsT m Type0
typeCheckOps e = do
    decls <- use (field @"decls")
    liftTypeT decls (inferType0 e)

bigStepOps :: Monad m => Exp0 Text -> OpsT m (Seq (Exp0 Text))
bigStepOps e = do
    defns <- use (field @"defns")
    liftEvalT defns (bigStep0 e)

freeVarsOps :: (Monad m, Ord a) => Exp0 a -> OpsT m (Set a)
freeVarsOps = pure . S.fromList . toList

-- closConvOps :: (Monad m, Eq n) => Exp n n -> OpsT m (ExpC n n)
-- closConvOps = liftConvT . closConv

-- lambdaLiftOps :: Monad m => ExpC Text -> OpsT m (ExpL Text)
-- lambdaLiftOps = liftLamLiftT . lambdaLift

-- fauxOps :: Monad m => FauxState Text -> ExpL Text -> OpsT m (ExpFC Text, FauxState Text)
-- fauxOps st = liftFauxT st . faux
