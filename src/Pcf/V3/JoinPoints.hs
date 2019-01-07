{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Pcf.V3.JoinPoints where

import Bound (Scope, abstract, instantiate1, makeBound)
import Control.Applicative (empty)
import Control.Lens (view)
import Control.Monad (ap, unless)
-- import Control.Monad.Cont (ContT, runContT)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Except (MonadError, throwError)
-- import Control.Monad.Trans (lift)
import Data.Constraint (Dict (..))
import Data.Deriving (deriveEq, deriveEq1, deriveShow, deriveShow1)
import Data.Foldable (traverse_)
import Data.Generics.Product (field)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Pcf.Core.BoundCrazy
import Pcf.Core.BoundUtil (localMod)
import Pcf.Core.Func (FuncT)

{-
IR where function application is specific
(for when functions can have greater arity)

call (fully saturated)
suspend (partially or fully saturated)
unsuspend ? (or maybe just call suspend is ok)
jump (to continuations)

(xor builtin, arity 2)
(def goOne (lam [a] (xor a)))
(def goTwo (lam [a b] (xor a b)))

will require a pass through definitions
-}

type Name = Text

data Type0 =
      TyBool0
    | TyArr0 (Vector Type0) Type0
    | TyCont0 Type0
    deriving (Eq, Show)

data Exp0 a =
      Var0 a
    | Call0 (Exp0 a) (Vector (Exp0 a))
    | Lam0 (Vector (Name, Type0)) (Scope Int Exp0 a)
    | Bool0 Bool
    | If0 (Exp0 a) (Exp0 a) (Exp0 a)
    | Control0 Name Type0 (Scope () Exp0 a)
    | Throw0 (Exp0 a) (Exp0 a)
    deriving (Functor, Foldable, Traversable)

instance Applicative Exp0 where
    pure = Var0
    (<*>) = ap

instance Monad Exp0 where
    return = pure
    (>>=) = mungeBind

instance Munge Exp0 where
    munge = undefined

$(deriveEq ''Exp0)
$(deriveShow ''Exp0)
$(deriveEq1 ''Exp0)
$(deriveShow1 ''Exp0)

-- $(makeBound ''Exp0)

-- data SuspType0 = CallSusp0 | JumpSusp0 deriving (Eq, Show)

-- data FunType0 = NonFun0 | CallFun0 | JumpFun0 | SuspFun0 SuspType0 Int deriving (Eq, Show)

-- funType0 :: Map Name FunType0 -> Exp0 Name -> Maybe FunType0
-- funType0 m (Var0 n) = M.lookup n m
-- funType0 _ (Lam0 nts _) = Just (SuspFun0 CallSusp0 (length nts))
-- funType0 m (Call0 e xs) = do
--     ft <- funType0 m e
--     let addl = length xs
--     case ft of
--         NonFun0 -> Nothing
--         CallFun0 -> Nothing
--         JumpFun0 -> Nothing
--         SuspFun0 st rem -> if | addl < rem -> Just (SuspFun0 st (rem - addl))
--                               | addl == rem -> Just (case st of { CallSusp0 -> CallFun0; JumpSusp0 -> JumpFun0 })
--                               | otherwise -> Nothing
-- funType0 m (Control0 n _ e) = let m' = M.insert n (SuspFun0 JumpSusp0 1) m in undefined
-- funType0 _ _ = Just NonFun0

data TypeError =
      TypeCheckError Type0 Type0
    | TypeMissingVarError Name
    | TypeUnboundVar Int
    | TypeTooManyArgs Int Int
    | TypeNotLambda
    | TypeNotCont
    deriving (Eq, Show)

data TypeEnv = TypeEnv
    { teTyMap :: Map Name Type0
    } deriving (Generic, Eq, Show)

type TypeC m = (MonadReader TypeEnv m, MonadError TypeError m)
type TypeT m a = FuncT TypeEnv () TypeError m a

typeProof :: Monad m => (forall n. TypeC n => n a) -> TypeT m a
typeProof = id

checkType0 :: TypeC m => Type0 -> Exp0 Name -> m ()
checkType0 t e = do
    u <- inferType0 e
    unless (u == t) (throwError (TypeCheckError u t))

insertAll :: (Foldable t, Ord a) => t (a, b) -> Map a b -> Map a b
insertAll nts m0 = foldl (\m (n, t) -> M.insert n t m) m0 nts

inferType0 :: TypeC m => Exp0 Name -> m Type0
inferType0 (Var0 n) = do
    tyMap <- view (field @"teTyMap")
    maybe (throwError (TypeMissingVarError n)) pure (M.lookup n tyMap)
inferType0 (Lam0 nts b) = do
    let k z = maybe (throwError (TypeUnboundVar z)) (pure . Var0 . fst) (nts V.!? z)
    e <- instantiateM k b
    et <- localMod (field @"teTyMap") (insertAll nts) (inferType0 e)
    let ts = snd <$> nts
    pure (TyArr0 ts et)
inferType0 (Call0 e xs) = do
    et <- inferType0 e
    case et of
        TyArr0 ats rty -> do
            let xlen = V.length xs
                alen = V.length ats
            if xlen > alen
                then throwError (TypeTooManyArgs xlen alen)
                else do
                    traverse_ (uncurry checkType0) (V.zip ats xs)
                    pure (if xlen == alen then rty else (TyArr0 (V.drop xlen ats) rty))
        _ -> throwError TypeNotLambda
inferType0 Bool0{} = pure TyBool0
inferType0 (If0 g t e) = do
    checkType0 TyBool0 g
    tt <- inferType0 t
    checkType0 tt e
    pure tt
inferType0 (Control0 n t b) = do
    let e = instantiate1 (Var0 n) b
    localMod (field @"teTyMap") (M.insert n (TyCont0 t)) (checkType0 t e)
    pure t
inferType0 (Throw0 c e) = do
    ct <- inferType0 c
    case ct of
        TyCont0 t -> checkType0 t e >> pure t
        _ -> throwError TypeNotCont

-- data EvalError =
--     EvalBoom
--     deriving (Eq, Show)

-- data EvalEnv = EvalEnv
--     { eeTmMap :: Map Name (Exp0 Name)
--     } deriving (Generic, Eq, Show)

-- type EvalC m = (MonadReader EvalEnv m, MonadError EvalError m)
-- type EvalT m a = FuncT EvalEnv () EvalError m a

-- evalProof :: Monad m => (forall n. EvalC n => n a) -> EvalT m a
-- evalProof = id

-- eval0 :: EvalC m => Exp0 Name -> m (Exp0 Name)
-- eval0 (Var0 n) = do
--     tmMap <- view (field @"eeTmMap")
--     maybe undefined pure (M.lookup n tmMap)
-- eval0 e@Lam0{} = pure e
-- eval0 (Call0 e xs) = do
--     xs' <- traverse eval0 xs
--     e' <- eval0 e
--     case e' of
--         Lam0 ns b -> undefined
--         _ -> undefined
-- eval0 e@Bool0{} = pure e
-- eval0 (If0 g t e) = do
--     g' <- eval0 g
--     case g' of
--         Bool0 True -> eval0 t
--         Bool0 False -> eval0 e
--         _ -> undefined
-- eval0 (Control0 n t b) = undefined
-- eval0 (Throw0 c e) = undefined

-- goOne and goAll would be represented

-- TODO actually bind variables
mkLam0 :: Vector (Name, Type0) -> Exp0 Name -> Exp0 Name
mkLam0 nts = let ns = fst <$> nts in Lam0 nts . abstract (flip V.elemIndex ns)

goOne0 :: Exp0 Name
goOne0 = mkLam0 (V.singleton ("a", TyBool0)) (Call0 (Var0 "xor") (V.singleton (Var0 "a")))

goAll0 :: Exp0 Name
goAll0 = mkLam0 (V.fromList [("a", TyBool0), ("b", TyBool0)]) (Call0 (Var0 "xor") (V.fromList [Var0 "a", Var0 "b"]))
