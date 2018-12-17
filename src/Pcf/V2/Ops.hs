module Pcf.V2.Ops where

import           Control.Lens               (assign, use)
import           Control.Monad              (unless)

import           Control.Monad.Except       (ExceptT, MonadError (..), runExceptT)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (runReaderT)
import           Control.Monad.State.Strict (MonadState (..), StateT, gets, runStateT)
import           Control.Monad.Trans        (MonadTrans (..))
import           Data.Generics.Product      (field)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import           Data.Set                   (Set)
import           Data.Text                  (Text)
import           Data.Void                  (absurd)
import           GHC.Generics               (Generic)
import           Pcf.Core.Func
import           Pcf.Core.Sub               (SubError, scopeFreeVars)
import           Pcf.V2.Functions
import           Pcf.V2.Parser              (readExp, readSExp, readStmt)
import           Pcf.V2.Types

data OpsData = OpsData
    { decls :: Map Text Ty
    , defns :: Map Text (Exp Text Text)
    } deriving (Generic, Eq, Show)

emptyOpsData :: OpsData
emptyOpsData = OpsData M.empty M.empty

-- TODO push some of these exceptions down into Functions
data OpsExc =
      AlreadyDeclared Text
    | NotDeclared Text
    | AlreadyDefined Text
    | CannotParseExp (SExp Text)
    | CannotParseStmt (SExp Text)
    | CannotParseSExp Text
    | WrapTypeError (TypeError Text)
    | WrapEvalError (EvalError Text Text)
    | WrapConvError SubError
    deriving (Generic, Eq, Show)

newtype OpsT m a = OpsT { unOps :: ExceptT OpsExc (StateT OpsData m) a }
    deriving (Functor, Applicative, Monad, MonadState OpsData, MonadError OpsExc, MonadIO)

instance MonadTrans OpsT where
    lift = OpsT . lift . lift

runOpsT :: OpsT m a -> OpsData -> m (Either OpsExc a, OpsData)
runOpsT ops = runStateT (runExceptT (unOps ops))

liftFuncT :: Monad m => r -> s -> (e -> OpsExc) -> (FuncT r s e m a) -> OpsT m (a, s)
liftFuncT env st wrap act = do
    mr <- lift (runFuncT act env st)
    either (throwError . wrap) pure mr

liftTypeT :: Monad m => (Map Text Ty) -> TypeT Text Text m b -> OpsT m b
liftTypeT tyMap = (fst <$>) . liftFuncT (TypeEnv pure tyMap) () WrapTypeError

liftEvalT :: Monad m => Map Text (Exp Text Text) -> EvalT Text Text m b -> OpsT m b
liftEvalT expMap = (fst <$>) . liftFuncT (EvalEnv pure expMap) () WrapEvalError

liftConvT :: Monad m => ConvT n n m b -> OpsT m b
liftConvT = (fst <$>) . liftFuncT (ConvEnv pure) () WrapConvError

-- liftLamLiftT :: Monad m => LamLiftT Text m b -> OpsT m b
-- liftLamLiftT = (fst <$>) . liftFuncT (LamLiftEnv pure) () WrapLamLiftError

-- liftFauxT :: Monad m => Monad m => FauxState Text -> FauxT Text m b -> OpsT m (b, FauxState Text)
-- liftFauxT st = liftFuncT () st WrapFauxError

parseSExp :: Monad m => Text -> OpsT m (SExp Text)
parseSExp input = maybe (throwError (CannotParseSExp input)) pure (readSExp input)

parseExp :: Monad m => SExp Text -> OpsT m (Exp Text Text)
parseExp se = maybe (throwError (CannotParseExp se)) pure (readExp se)

parseStmt :: Monad m => SExp Text -> OpsT m (Stmt Text Text)
parseStmt se = maybe (throwError (CannotParseStmt se)) pure (readStmt se)

declare :: Monad m => Text -> Ty -> OpsT m ()
declare name ty = do
    decls <- use (field @"decls")
    if M.member name decls
        then throwError (AlreadyDeclared name)
        else pure ()
    let decls' = M.insert name ty decls
    assign (field @"decls") decls'
    pure ()

define :: Monad m => Text -> Exp Text Text -> OpsT m ()
define name e = do
    decls <- use (field @"decls")
    case M.lookup name decls of
        Nothing -> throwError (NotDeclared name)
        Just expectedTy -> do
            defns <- use (field @"defns")
            if M.member name defns
                then throwError (AlreadyDefined name)
                else pure ()
            actualTy <- liftTypeT (M.delete name decls) (assertTy expectedTy e)
            let defns' = M.insert name e defns
            assign (field @"defns") defns'

processStmt :: Monad m => Stmt Text Text -> OpsT m ()
processStmt (Decl name ty)  = declare name ty
processStmt (Defn name exp) = define name exp

typeCheckOps :: Monad m => Exp Text Text -> OpsT m Ty
typeCheckOps e = do
    decls <- use (field @"decls")
    liftTypeT decls (typeCheck e)

bigStepOps :: Monad m => Exp Text Text -> OpsT m (Exp Text Text)
bigStepOps e = do
    defns <- use (field @"defns")
    liftEvalT defns (bigStep e)

freeVarsOps :: (Monad m, Ord a) => Exp n a -> OpsT m (Set a)
freeVarsOps = pure . scopeFreeVars

closConvOps :: (Monad m, Eq n) => Exp n n -> OpsT m (ExpC n n)
closConvOps = liftConvT . closConv

-- lambdaLiftOps :: Monad m => ExpC Text -> OpsT m (ExpL Text)
-- lambdaLiftOps = liftLamLiftT . lambdaLift

-- fauxOps :: Monad m => FauxState Text -> ExpL Text -> OpsT m (ExpFC Text, FauxState Text)
-- fauxOps st = liftFauxT st . faux
