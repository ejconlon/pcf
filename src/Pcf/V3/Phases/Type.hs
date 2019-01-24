module Pcf.V3.Phases.Type where

import           Bound                 (Scope, instantiate, instantiate1)
import           Control.Applicative   (empty)
import           Control.Lens          (Lens', assign, modifying, use, view)
import           Control.Monad         (unless)
import           Control.Monad.Except  (MonadError (..))
import           Control.Monad.Reader  (MonadReader, ask)
import           Control.Monad.State   (MonadState (..), modify)
import           Data.Foldable         (traverse_)
import           Data.Functor          (($>))
import           Data.Generics.Product (field)
import           Data.Map              (Map)
import qualified Data.Map              as M
import           Data.Sequence         (Seq (..), (|>))
import qualified Data.Sequence         as Seq
import           Data.Text             (Text)
import qualified Data.Text             as T
import           GHC.Generics          (Generic)
import           Pcf.Core.BoundCrazy   (instantiateM)
import           Pcf.Core.Func         (FuncT)
import           Pcf.Core.Util         (insertAll, izipWithM_, localMod, filterMap)
import           Pcf.V3.Types

-- Typing

data TypeError =
      TypeCheckError Type0 Type0
    | TypePatCheckError Type0 Type0 Type0
    | TypeMissingVarError Name
    | TypeUnboundVarError Int
    | TypeLamArityError Int Int
    | TypeConArityError Name Int Int
    | TypeNotLamError
    | TypeNotDataError
    | TypeNotContError
    | TypeEmptyCaseError
    | TypeUnknownConError Name
    | TypeConMismatchError Name Name Name
    deriving (Generic, Eq, Show)

data TypeEnv = TypeEnv
    { teTyMap    :: Map Name Type0
    , teDataDefs :: DataDefs Type0
    , tePath     :: Path0
    } deriving (Generic, Eq, Show)

type FullTypeError = PathError Path0 TypeError

type TypeC m = (MonadReader TypeEnv m, MonadError FullTypeError m)
type TypeT m a = FuncT TypeEnv () FullTypeError m a

typeProof :: Monad m => (forall n. TypeC n => n a) -> TypeT m a
typeProof = id

throwTypePathError :: TypeC m => TypeError -> m a
throwTypePathError = throwPathError (view (field @"tePath"))

typeWithDir :: MonadReader TypeEnv m => Dir0 -> m z -> m z
typeWithDir d = localMod (field @"tePath") (flip (|>) d)

checkPatType0 :: TypeC m => Type0 -> Type0 -> Pat0 Name -> m ()
checkPatType0 inTy outTy p = do
    u <- inferPatType0 inTy p
    unless (u == outTy) (throwTypePathError (TypePatCheckError inTy u outTy))

inferPatType0 :: TypeC m => Type0 -> Pat0 Name -> m Type0
inferPatType0 inTy p =
    case p of
        VarPat0 n b -> do
            let e = instantiate1 (Var0 n) b
            localMod (field @"teTyMap") (M.insert n inTy) (inferType0 e)
        ConPat0 n is b -> do
            dds <- view (field @"teDataDefs")
            case M.lookup n (conNameToTyNameAndDef dds) of
                Nothing -> throwTypePathError (TypeUnknownConError n)
                Just (w', cd) ->
                    case inTy of
                        TyCon0 w -> do
                            unless (w' == w) (throwTypePathError (TypeConMismatchError w w' n))
                            let k z = case Seq.lookup z is of
                                    Just (ConcreteIdent n) -> pure (Var0 n)
                                    _ -> throwTypePathError (TypeUnboundVarError z)
                            e <- instantiateM k b
                            let nts = filterMap nameFilter2 (Seq.zip is (conDefTypes cd))
                            localMod (field @"teTyMap") (insertAll nts) (inferType0 e)
                        _ -> throwTypePathError TypeNotDataError
        WildPat0 e -> inferType0 e

checkType0 :: TypeC m => Type0 -> Exp0 Name -> m ()
checkType0 t e = do
    u <- inferType0 e
    unless (u == t) (throwTypePathError (TypeCheckError u t))

inferTyFun0 :: TypeC m => Exp0 Name -> m (Seq Type0, Type0)
inferTyFun0 e = do
    et <- inferType0 e
    case et of
        TyFun0 ats rty -> pure (ats, rty)
        _              -> throwTypePathError TypeNotLamError

inferTyCont0 :: TypeC m => Exp0 Name -> m Type0
inferTyCont0 e = do
    et <- inferType0 e
    case et of
        TyCont0 t -> pure t
        _         -> throwTypePathError TypeNotContError

inferType0 :: TypeC m => Exp0 Name -> m Type0
inferType0 (Var0 n) = do
    tyMap <- view (field @"teTyMap")
    maybe (throwTypePathError (TypeMissingVarError n)) pure (M.lookup n tyMap)
inferType0 (Let0 i e b) = do
    t <- typeWithDir DirLetArg0 (inferType0 e)
    typeWithDir DirLetBody0 $ do
        tyMap <- view (field @"teTyMap")
        let (m, k) = case i of
                ConcreteIdent n -> (M.insert n t tyMap, const (pure (Var0 n)))
                WildIdent -> (tyMap, const (throwTypePathError (TypeUnboundVarError 0)))
        u <- instantiateM k b
        localMod (field @"teTyMap") (const m) (inferType0 u)
inferType0 (Con0 n xs) = do
    dds <- view (field @"teDataDefs")
    case M.lookup n (conNameToTyNameAndDef dds) of
        Nothing -> throwTypePathError (TypeUnknownConError n)
        Just (tyName, conDef) -> do
            let ats = conDefTypes conDef
                xlen = Seq.length xs
                alen = Seq.length ats
                rty = TyCon0 tyName
            if xlen /= alen
                then throwTypePathError (TypeConArityError n xlen alen)
                else do
                    izipWithM_ (\i at x -> typeWithDir (DirConArg0 i) (checkType0 at x)) ats xs
                    pure rty
inferType0 (Case0 e ps) = do
    t <- typeWithDir DirCaseArg0 (inferType0 e)
    case ps of
        Seq.Empty -> throwTypePathError (TypeEmptyCaseError)
        p :<| ps -> do
            pt <- typeWithDir (DirCasePat0 0) (inferPatType0 t p)
            Seq.traverseWithIndex (\i q -> typeWithDir (DirCasePat0 (i+1)) (checkPatType0 t pt q)) ps
            pure pt
inferType0 (Call0 e xs) = do
    (ats, rty) <- typeWithDir DirCallFun0 (inferTyFun0 e)
    let xlen = Seq.length xs
        alen = Seq.length ats
    if xlen /= alen
        then throwTypePathError (TypeLamArityError xlen alen)
        else do
            izipWithM_ (\i at x -> typeWithDir (DirCallArg0 i) (checkType0 at x)) ats xs
            pure rty
inferType0 (Lam0 its b) = do
    let nts = filterMap nameFilter2 its
    et <- typeWithDir DirLamBody0 $ do
        let k z = case Seq.lookup z its of
                Just (ConcreteIdent n, _) -> pure (Var0 n)
                _ -> throwTypePathError (TypeUnboundVarError z)
        e <- instantiateM k b
        localMod (field @"teTyMap") (insertAll nts) (inferType0 e)
    let ts = snd <$> nts
    pure (TyFun0 ts et)
inferType0 (CallCC0 n t b) = do
    let e = instantiate1 (Var0 n) b
    localMod (field @"teTyMap") (M.insert n (TyCont0 t)) (typeWithDir DirCallCCBody0 (checkType0 t e))
    pure t
inferType0 (Throw0 c e) = do
    t <- typeWithDir DirThrowFun0 (inferTyCont0 c)
    typeWithDir DirThrowArg0 (checkType0 t e)
    pure t
inferType0 (The0 e t) = typeWithDir DirThe0 (checkType0 t e) $> t
