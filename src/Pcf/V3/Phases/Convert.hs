module Pcf.V3.Phases.Convert where

import           Bound                 (abstract, abstract1)
import           Control.Lens          (view)
import           Control.Monad         (unless)
import           Control.Monad.Except  (MonadError (throwError))
import           Control.Monad.Reader  (MonadReader)
import           Data.Generics.Product (field)
import qualified Data.Map              as M
import           Data.Sequence         (Seq)
import qualified Data.Sequence         as Seq
import           GHC.Generics          (Generic)
import           Pcf.Core.Func
import           Pcf.V3.Types

data ConvertEnv t = ConvertEnv
    { ceDataDefs :: DataDefs t
    } deriving (Generic, Show, Eq)

data ConvertError =
      ConvertUnknownTypeError Name
    | ConvertUnknownConError Name
    | ConvertConArityMismatch Name Int Int
    deriving (Generic, Show, Eq)

type FullConvertError = ConvertError

type ConvertC t m = (MonadReader (ConvertEnv t) m, MonadError FullConvertError m)
type ConvertT t m a = FuncT (ConvertEnv t) () FullConvertError m a

convertProof :: Monad m => (forall n. ConvertC t n => n a) -> ConvertT t m a
convertProof = id

convertTy :: ConvertC t m => TypeX -> m Type0
convertTy ty =
    case ty of
        TyVarX n -> do
            dds <- view (field @"ceDataDefs")
            if declaredDataType n dds
                then pure (TyCon0 n)
                else throwError (ConvertUnknownTypeError n)
        TyFunX xs r -> TyFun0 <$> traverse convertTy xs <*> convertTy r
        TyContX t -> TyCont0 <$> convertTy t

lookupCon :: ConvertC t m => Name -> m (Maybe (ConDef t))
lookupCon n = do
    dds <- view (field @"ceDataDefs")
    let mcd = snd <$> M.lookup n (conNameToTyNameAndDef dds)
    pure mcd

getCon :: ConvertC t m => Name -> m (ConDef t)
getCon n = do
    mcd <- lookupCon n
    maybe (throwError (ConvertUnknownConError n)) pure mcd

assertArityMatch :: ConvertC t m => ConDef t -> Int -> m ()
assertArityMatch (ConDef n ts) i =
    let j = Seq.length ts
    in unless (j == i) (throwError (ConvertConArityMismatch n j i))

convertPat :: ConvertC t m => PatX -> m (Pat0 Name)
convertPat (PatX p e) =
    case p of
        VarPatL i ->
            let e' = convertExp e
            in case i of
                ConcreteIdent n -> (\e'' -> VarPat0 n (abstract1 n e'')) <$> e'
                WildIdent -> WildPat0 <$> e'
        ConPatL n is -> do
            cd <- getCon n
            assertArityMatch cd (Seq.length is)
            let k a = Seq.findIndexR (\i -> i == ConcreteIdent a) is
            e' <- convertExp e
            let e'' = abstract k e'
            pure (ConPat0 n is e'')

convertConDef :: ConvertC t m => ConDefX -> m ConDef0
convertConDef (ConDef n ts) = ConDef n <$> traverse convertTy ts

convertCon :: ConvertC t m => ConDef t -> Seq ExpX -> m (Exp0 Name)
convertCon (ConDef n ts) xs =
    let xlen = Seq.length xs
        alen = Seq.length ts
    in if | xlen /= alen -> throwError (ConvertConArityMismatch n alen xlen)
          | otherwise -> Con0 n <$> traverse convertExp xs

convertCall :: ConvertC t m => ExpX -> Seq ExpX -> m (Exp0 Name)
convertCall e xs = Call0 <$> convertExp e <*> traverse convertExp xs

convertExp :: ConvertC t m => ExpX -> m (Exp0 Name)
convertExp ex =
    case ex of
        VarX n -> do
            mcd <- lookupCon n
            maybe (pure (Var0 n)) (flip convertCon Seq.empty) mcd
        LetX i e u -> Let0 i <$> convertExp e <*> u'' where
            u' = convertExp u
            k = case i of
                ConcreteIdent y -> \a -> if y == a then Just () else Nothing
                WildIdent -> const Nothing
            u'' = abstract k <$> u'
        CaseX e ps -> Case0 <$> convertExp e <*> traverse convertPat ps
        CallX e xs ->
            case e of
                VarX n -> do
                    mcd <- lookupCon n
                    case mcd of
                        Just cd -> convertCon cd xs
                        Nothing -> convertCall e xs
                _ -> convertCall e xs
        LamX its e -> Lam0 <$> its' <*> e'' where
            its' = traverse (\(i, t) -> (i,) <$> convertTy t) its
            e' = convertExp e
            k a = Seq.findIndexR (\(i, _) -> i == ConcreteIdent a) its
            e'' = abstract k <$> e'
        CallCCX n ty e -> CallCC0 n <$> convertTy ty <*> e'' where
            e' = convertExp e
            e'' = abstract1 n <$> e'
        ThrowX c e -> Throw0 <$> convertExp c <*> convertExp e
        TheX e ty -> The0 <$> convertExp e <*> convertTy ty

convertStmt :: ConvertC t m => StmtX -> m Stmt0
convertStmt s =
    case s of
        Decl n t -> Decl n <$> convertTy t
        Defn n e -> Defn n <$> convertExp e
        Data n cds -> Data n <$> traverse convertConDef cds
