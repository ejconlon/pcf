module Pcf.Ops where

import           Control.Lens               (assign, use)
import           Control.Monad              (unless)
import           Control.Monad.Catch        (Exception, MonadThrow (..))
import           Control.Monad.Except       (ExceptT, MonadError (..), runExceptT)
import           Control.Monad.State.Strict (MonadState (..), State, gets, runState)
import           Data.Generics.Product      (field)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as Map
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import           Data.Typeable              (Typeable)
import           GHC.Generics               (Generic)
import           Pcf.Functions              (bigStep, typeCheck)
import           Pcf.Parser                 (readExp, readSExp, readStmt)
import           Pcf.Types                  (Exp (..), SExp (..), Stmt (..), Ty (..))

data OpsData = OpsData
    { decls :: Map Text Ty
    , defns :: Map Text (Exp Text)
    -- TODO also include dependencies between decls and between defns
    } deriving (Generic, Eq, Show)

emptyOpsData :: OpsData
emptyOpsData = OpsData Map.empty Map.empty

data OpsExc =
      AlreadyDeclared Text
    | NotDeclared Text
    | AlreadyDefined Text
    | CannotType (Exp Text)
    | CannotEval (Exp Text)
    | TypeMismatch (Exp Text) Ty Ty
    | CannotParseExp (SExp Text)
    | CannotParseStmt (SExp Text)
    | CannotParseSExp Text
    deriving (Generic, Eq, Show, Typeable)
instance Exception OpsExc

newtype Ops a = Ops { unOps :: ExceptT OpsExc (State OpsData) a }
    deriving (Functor, Applicative, Monad, MonadState OpsData, MonadError OpsExc)

runOps :: Ops a -> OpsData -> (Either OpsExc a, OpsData)
runOps ops = runState (runExceptT (unOps ops))

interpretOps :: (MonadState OpsData m, MonadThrow m) => Ops a -> m a
interpretOps ops = do
    dat <- get
    let (ea, dat') = runOps ops dat
    put dat'
    case ea of
        Left e  -> throwM e
        Right a -> pure a

declare :: Text -> Ty -> Ops ()
declare name ty = do
    decls <- use (field @"decls")
    if Map.member name decls
        then throwError (AlreadyDeclared name)
        else pure ()
    let decls' = Map.insert name ty decls
    assign (field @"decls") decls'
    pure ()

define :: Text -> Exp Text -> Ops ()
define name e = do
    decls <- use (field @"decls")
    case Map.lookup name decls of
        Nothing -> throwError (NotDeclared name)
        Just expectedTy -> do
            defns <- use (field @"defns")
            if Map.member name defns
                then throwError (AlreadyDefined name)
                else pure ()
            case typeCheck (Map.delete name decls) e of
                Nothing -> throwError (CannotType e)
                Just actualTy ->
                    unless (actualTy /= expectedTy) (throwError (TypeMismatch e expectedTy actualTy))
            let defns' = Map.insert name e defns
            assign (field @"defns") defns'

process :: Stmt Text -> Ops ()
process (Decl name ty)  = declare name ty
process (Defn name exp) = define name exp

typeCheckOps :: Exp Text -> Ops Ty
typeCheckOps e = do
    decls <- use (field @"decls")
    case typeCheck decls e of
        Nothing -> throwError (CannotType e)
        Just ty -> pure ty

bigStepOps :: Exp Text -> Ops (Exp Text)
bigStepOps e = do
    defns <- use (field @"defns")
    case bigStep defns e of
        Nothing -> throwError (CannotEval e)
        Just e' -> pure e'

parseSExp :: Text -> Ops (SExp Text)
parseSExp input = maybe (throwError (CannotParseSExp input)) pure (readSExp input)

parseExp :: SExp Text -> Ops (Exp Text)
parseExp se = maybe (throwError (CannotParseExp se)) pure (readExp se)

parseStmt :: SExp Text -> Ops (Stmt Text)
parseStmt se = maybe (throwError (CannotParseStmt se)) pure (readStmt se)

clear :: Ops ()
clear = put emptyOpsData