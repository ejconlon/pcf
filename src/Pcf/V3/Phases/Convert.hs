module Pcf.V3.Phases.Convert where

import           Bound                 (Scope)
import           Control.Lens          (view)
import           Control.Monad         (unless, when)
import           Control.Monad.Except  (MonadError (throwError))
import           Control.Monad.Reader  (MonadReader)
import           Control.Monad.Trans   (lift)
import           Data.Generics.Product (field)
import qualified Data.Map              as M
import           Data.Maybe            (isJust)
import           Data.Sequence         (Seq)
import qualified Data.Sequence         as Seq
import           GHC.Generics          (Generic)
import           Pcf.Core.BoundCrazy   (abstracting, abstracting1)
import           Pcf.Core.Func
import           Pcf.Core.Util         (ensure)
import           Pcf.V3.Names
import           Pcf.V3.Types

data ConvertEnv t = ConvertEnv
    { ceDataDefs :: DataDefs t
    } deriving (Generic, Show, Eq)

data ConvertError =
      ConvertUnknownTypeError Name
    | ConvertUnknownConError Name
    | ConvertShadowConError Name
    deriving (Generic, Show, Eq)

type ConvertC t m = (MonadReader (ConvertEnv t) m, MonadError ConvertError m)
newtype ConvertT t m a = ConvertT { unConvertT :: FuncT (ConvertEnv t) () ConvertError m a }
    deriving (Functor, Applicative, Monad, MonadReader (ConvertEnv t), MonadError ConvertError)

convertProof :: Monad m => (forall n. ConvertC t n => n a) -> ConvertT t m a
convertProof = id

lookupCon :: ConvertC t m => Name -> m (Maybe (ConDef t))
lookupCon n = do
    dds <- view (field @"ceDataDefs")
    pure (snd <$> M.lookup n (conNameToTyNameAndDef dds))

conExists :: ConvertC t m => Name -> m Bool
conExists n = isJust <$> lookupCon n

getCon :: ConvertC t m => Name -> m (ConDef t)
getCon n = do
    mcd <- lookupCon n
    maybe (throwError (ConvertUnknownConError n)) pure mcd

tyExists :: ConvertC t m => Name -> m Bool
tyExists n = do
    dds <- view (field @"ceDataDefs")
    pure (declaredDataType n dds)

convertTy :: ConvertC t m => TypeX -> m Type0
convertTy ty =
    case ty of
        TyVarX n -> do
            exists <- tyExists n
            if exists
                then pure (TyCon0 n)
                else throwError (ConvertUnknownTypeError n)
        TyFunX xs r -> TyFun0 <$> traverse convertTy xs <*> convertTy r
        TyContX t -> TyCont0 <$> convertTy t

convertPat :: ConvertC t m => PatX -> m (Pat0 Name)
convertPat (PatX p e) =
    case p of
        VarPatL i -> do
            e' <- convertExp e
            case i of
                ConcreteIdent n -> do
                    exists <- conExists n
                    let e'' = if exists
                            then ConPat0 n Seq.empty (lift e')
                            else VarPat0 (ConcreteIdent n) (abstracting1 n e')
                    pure e''
                WildIdent       -> pure (VarPat0 WildIdent (lift e'))
        ConPatL n is -> do
            cd <- getCon n
            traverse validateVarIdent is
            e' <- convertExp e
            let sks = projectSubKs is
                e'' = abstracting sks e'
            pure (ConPat0 n is e'')

convertConDef :: ConvertC t m => ConDefX -> m ConDef0
convertConDef (ConDef n ts) = ConDef n <$> traverse convertTy ts

convertCon :: ConvertC t m => Name -> Seq ExpX -> m (Exp0 Name)
convertCon n xs = Con0 n <$> traverse convertExp xs

convertCall :: ConvertC t m => ExpX -> Seq ExpX -> m (Exp0 Name)
convertCall e xs = Call0 <$> convertExp e <*> traverse convertExp xs

validateVarName :: ConvertC t m => Name -> m ()
validateVarName n = do
    exists <- conExists n
    when exists (throwError (ConvertShadowConError n))

validateVarIdent :: ConvertC t m => Ident -> m ()
validateVarIdent i =
    case i of
        ConcreteIdent n -> validateVarName n
        WildIdent -> pure ()

convertExp :: ConvertC t m => ExpX -> m (Exp0 Name)
convertExp ex =
    case ex of
        VarX n -> do
            exists <- conExists n
            if exists then convertCon n Seq.empty else pure (Var0 n)
        LetX i e u -> do
            validateVarIdent i
            e' <- convertExp e
            u' <- convertExp u
            let sks = projectSubK i
                b = abstracting sks u'
            pure (Let0 i e' b)
        CaseX e ps -> Case0 <$> convertExp e <*> traverse convertPat ps
        CallX e xs ->
            case e of
                VarX n -> do
                    exists <- conExists n
                    if exists then convertCon n xs else convertCall e xs
                _ -> convertCall e xs
        LamX its e -> do
            its' <- traverse (\(i, t) -> (,) <$> ensure validateVarIdent i <*> convertTy t) its
            e' <- convertExp e
            let sks = projectSubKs (fst <$> its')
                b = abstracting sks e'
            pure (Lam0 its' b)
        CallCCX i ty e -> do
            validateVarIdent i
            ty' <- convertTy ty
            e' <- convertExp e
            let sks = projectSubK i
                b = abstracting sks e'
            pure (CallCC0 i ty' b)
        ThrowX c e -> Throw0 <$> convertExp c <*> convertExp e
        TheX e ty -> The0 <$> convertExp e <*> convertTy ty

convertStmt :: ConvertC t m => StmtX -> m Stmt0
convertStmt s =
    case s of
        Decl n t   -> Decl n <$> convertTy t
        Defn n e   -> Defn n <$> convertExp e
        Data n cds -> Data n <$> traverse convertConDef cds
