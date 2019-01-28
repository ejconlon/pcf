module Pcf.V3.Phases.Type where

import           Bound                 (Scope)
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
import           Pcf.Core.BoundCrazy   (Instantiable (..), Sub, handle, instantiateE)
import           Pcf.Core.Func         (FuncT)
import           Pcf.Core.Util         (filterMap, insertAll, izipWithM_, localMod)
import           Pcf.V3.Names
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

type TypeC m = (Instantiable m, MonadReader TypeEnv m, MonadError FullTypeError m)
newtype TypeT m a = TypeT { unTypeT :: FuncT TypeEnv () FullTypeError m a }
    deriving (Functor, Applicative, Monad, MonadReader TypeEnv, MonadError FullTypeError)

instance Monad m => Instantiable (TypeT m) where
    instantiating = instantiateE (throwTypePathError . TypeUnboundVarError)

exploding :: (Instantiable m, MonadReader TypeEnv m) => Seq (Sub Int Name Type0) -> Scope Int Exp0 Name -> (Exp0 Name -> m a) -> m a
exploding = handle (field @"teTyMap")

typeProof :: Monad m => (forall n. TypeC n => n a) -> TypeT m a
typeProof = id

makeTypePathError :: TypeC m => TypeError -> m FullTypeError
makeTypePathError e = PathError <$> view (field @"tePath") <*> pure e

throwTypePathError :: TypeC m => TypeError -> m a
throwTypePathError e = makeTypePathError e >>= throwError

typeWithDir :: MonadReader TypeEnv m => Dir0 -> m z -> m z
typeWithDir d = localMod (field @"tePath") (flip (|>) d)

checkPatType0 :: TypeC m => Type0 -> Type0 -> Pat0 Name -> m ()
checkPatType0 inTy outTy p = do
    u <- inferPatType0 inTy p
    unless (u == outTy) (throwTypePathError (TypePatCheckError inTy u outTy))

inferPatType0 :: TypeC m => Type0 -> Pat0 Name -> m Type0
inferPatType0 inTy p =
    case p of
        VarPat0 i b -> exploding (projectSub (i, inTy)) b inferType0
        ConPat0 n is b -> do
            dds <- view (field @"teDataDefs")
            case M.lookup n (conNameToTyNameAndDef dds) of
                Nothing -> throwTypePathError (TypeUnknownConError n)
                Just (w', cd) ->
                    case inTy of
                        TyCon0 w -> do
                            unless (w' == w) (throwTypePathError (TypeConMismatchError w w' n))
                            exploding (projectSubs (Seq.zip is (conDefTypes cd))) b inferType0
                        _ -> throwTypePathError TypeNotDataError

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
    typeWithDir DirLetBody0 (exploding (projectSub (i, t)) b inferType0)
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
    et <- typeWithDir DirLamBody0 (exploding (projectSubs its) b inferType0)
    let nts = selectSubVs its
        ts = snd <$> nts
    pure (TyFun0 ts et)
inferType0 (CallCC0 i t b) = do
    typeWithDir DirCallCCBody0 (exploding (projectSub (i, TyCont0 t)) b (checkType0 t))
    pure t
inferType0 (Throw0 c e) = do
    t <- typeWithDir DirThrowFun0 (inferTyCont0 c)
    typeWithDir DirThrowArg0 (checkType0 t e)
    pure t
inferType0 (The0 e t) = typeWithDir DirThe0 (checkType0 t e) $> t
