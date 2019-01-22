{-# LANGUAGE Rank2Types #-}

module Pcf.V3.Functions where

import           Bound                 (Scope, instantiate1)
import           Control.Applicative   (empty)
import           Control.Lens          (Lens', assign, modifying, use, view)
import           Control.Monad         (unless)
import           Control.Monad.Except  (MonadError (..))
import           Control.Monad.Reader  (MonadReader, ask)
import           Control.Monad.State   (MonadState (..), modify)
import           Data.Foldable         (traverse_)
import           Data.Functor          (($>))
import           Data.Generics.Product (field)
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as M
import           Data.Sequence         (Seq (..), (|>))
import qualified Data.Sequence         as Seq
import           Data.Text             (Text)
import qualified Data.Text             as T
import           GHC.Generics          (Generic)
import           Pcf.Core.BoundCrazy   (instantiateM)
import           Pcf.Core.Func         (FuncT)
import           Pcf.Core.Util         (izipWithM_, localMod)
import           Pcf.V3.Types

-- Utils

insertAll :: (Foldable t, Ord a) => t (a, b) -> Map a b -> Map a b
insertAll nts m0 = foldl (\m (n, t) -> M.insert n t m) m0 nts

-- Stuff

data PathError p e = PathError
    { eePath  :: p
    , eeError :: e
    } deriving (Generic, Eq, Show)

throwPathError :: MonadError (PathError p e) m => m p -> e -> m z
throwPathError mp e = do
    p <- mp
    throwError (PathError p e)

-- Defs

data DataDefs = DataDefs
    { tyNameToConNames :: Map Text (Seq Text)
    , conNameToTyNameAndDef :: Map Text (Text, ConDef0)
    } deriving (Generic, Eq, Show)

declaredDataType :: Name -> DataDefs -> Bool
declaredDataType n dds = M.member n (tyNameToConNames dds)

addDataDef :: Name -> Seq ConDef0 -> DataDefs -> DataDefs
addDataDef n cds (DataDefs x y) = DataDefs x' y' where
    x' = M.insert n (conDefName <$> cds) x
    y' = foldl (\z cd -> M.insert (conDefName cd) (n, cd) z) y cds

emptyDataDefs :: DataDefs
emptyDataDefs = DataDefs M.empty M.empty

-- Typing

data TypeError =
      TypeCheckError Type0 Type0
    | TypePatCheckError Type0 Type0 Type0
    | TypeMissingVarError Name
    | TypeUnboundVarError Int
    | TypeTooManyArgsError Int Int
    | TypeConTooManyArgsError Name Int Int
    | TypeNotFunError
    | TypeNotDataError
    | TypeNotContError
    | TypeEmptyCaseError
    | TypeUnknownConError Name
    | TypeConMismatchError Name Name Name
    deriving (Generic, Eq, Show)

data TypeEnv = TypeEnv
    { teTyMap    :: Map Name Type0
    , teDataDefs :: DataDefs
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
        ConPat0 n ns b -> do
            dds <- view (field @"teDataDefs")
            case M.lookup n (conNameToTyNameAndDef dds) of
                Nothing -> throwTypePathError (TypeUnknownConError n)
                Just (w', cd) ->
                    case inTy of
                        TyCon0 w -> do
                            unless (w' == w) (throwTypePathError (TypeConMismatchError w w' n))
                            let k z = maybe (throwTypePathError (TypeUnboundVarError z)) (pure . Var0) (Seq.lookup z ns)
                            e <- instantiateM k b
                            let nts = Seq.zip ns (conDefTypes cd)
                            localMod (field @"teTyMap") (insertAll nts) (inferType0 e)
                        _ -> throwTypePathError TypeNotDataError
        WildPat0 e -> inferType0 e

checkType0 :: TypeC m => Type0 -> Exp0 Name -> m ()
checkType0 t e = do
    u <- inferType0 e
    unless (u == t) (throwTypePathError (TypeCheckError u t))

inferTyArr0 :: TypeC m => Exp0 Name -> m (Seq Type0, Type0)
inferTyArr0 e = do
    et <- inferType0 e
    case et of
        TyFun0 ats rty -> pure (ats, rty)
        _              -> throwTypePathError TypeNotFunError

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
inferType0 (Let0 n e b) = do
    t <- typeWithDir DirLetArg0 (inferType0 e)
    let u = instantiate1 (Var0 n) b
    typeWithDir DirLetBody0 (localMod (field @"teTyMap") (M.insert n t) (inferType0 u))
inferType0 (Con0 n xs) = do
    dds <- view (field @"teDataDefs")
    case M.lookup n (conNameToTyNameAndDef dds) of
        Nothing -> throwTypePathError (TypeUnknownConError n)
        Just (tyName, conDef) -> do
            let ats = conDefTypes conDef
                xlen = Seq.length xs
                alen = Seq.length ats
                rty = TyCon0 n
            if xlen > alen
                then throwTypePathError (TypeConTooManyArgsError n xlen alen)
                else do
                    izipWithM_ (\i at x -> typeWithDir (DirConArg0 i) (checkType0 at x)) ats xs
                    pure (if xlen == alen then rty else (TyFun0 (Seq.drop xlen ats) rty))
inferType0 (Case0 e ps) = do
    t <- typeWithDir DirCaseArg0 (inferType0 e)
    case ps of
        Seq.Empty -> throwTypePathError (TypeEmptyCaseError)
        p :<| ps -> do
            pt <- typeWithDir (DirCasePat0 0) (inferPatType0 t p)
            Seq.traverseWithIndex (\i q -> typeWithDir (DirCasePat0 (i+1)) (checkPatType0 t pt q)) ps
            pure pt
inferType0 (Call0 e xs) = do
    (ats, rty) <- typeWithDir DirCallFun0 (inferTyArr0 e)
    let xlen = Seq.length xs
        alen = Seq.length ats
    if xlen > alen
        then throwTypePathError (TypeTooManyArgsError xlen alen)
        else do
            izipWithM_ (\i at x -> typeWithDir (DirCallArg0 i) (checkType0 at x)) ats xs
            pure (if xlen == alen then rty else (TyFun0 (Seq.drop xlen ats) rty))
inferType0 (Lam0 nts b) = do
    let k z = maybe (throwTypePathError (TypeUnboundVarError z)) (pure . Var0 . fst) (Seq.lookup z nts)
    et <- typeWithDir DirLamBody0 $ do
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

-- -- Evaluation

-- data Kont0 =
--     KontTop0
--   | KontCallFun0 (Seq (Exp0 Name)) EvalState
--   | KontCallArg0 (Exp0 Name) (Seq (Exp0 Name)) (Seq (Exp0 Name)) EvalState
--   | KontCaseTarget0 (Seq (Pat0 Name)) EvalState
--   | KontCasePat0 (Seq (Pat0 Name)) EvalState
--   | KontThrowFun0 (Exp0 Name) EvalState
--   | KontThrowArg0 (Exp0 Name) EvalState
--   deriving (Eq, Show)

-- data EvalError =
--       EvalTopError
--     | EvalUnboundVarError Int
--     | EvalMissingContError Name
--     | EvalTooManyArgsError Int Int
--     | EvalNotLambdaError
--     | EvalNotBoolError
--     | EvalNotControlError
--     | EvalUnmatchedCaseError
--     deriving (Generic, Eq, Show)

-- data EvalTerm =
--       KontTerm
--     | ExpTerm (Exp0 Name)
--     deriving (Generic, Eq, Show)

-- data EvalState = EvalState
--     { esKont  :: Kont0
--     , esStack :: Seq (Name, EvalState)
--     , esTmMap :: Map Name EvalTerm
--     } deriving (Generic, Eq, Show)

-- type EvalC m = (MonadState EvalState m, MonadError EvalError m)
-- type EvalT m a = FuncT () EvalState EvalError m a

-- evalProof :: Monad m => (forall n. EvalC n => n a) -> EvalT m a
-- evalProof = id

-- looking :: MonadError EvalError m => Seq Name -> (Int -> m (Exp0 Name))
-- looking xs i =
--     case Seq.lookup i xs of
--         Nothing -> throwError (EvalUnboundVarError i)
--         Just x  -> pure (Var0 x)

-- call0 :: EvalC m => Seq Name -> Seq (Exp0 Name) -> Scope Int Exp0 Name -> m (Maybe (Exp0 Name))
-- call0 ns xs b =
--     let xlen = Seq.length xs
--         nlen = Seq.length ns
--     in if | xlen > nlen -> throwError (EvalTooManyArgsError xlen nlen)
--           | xlen < nlen -> pure Nothing
--           | otherwise -> do
--                 shiftKont0
--                 modifying (field @"esTmMap") (\m -> foldl (\m' (n, x) -> M.insert n (ExpTerm x) m) m (Seq.zip ns xs))
--                 Just <$> instantiateM (looking ns) b

-- kontState :: Kont0 -> Maybe EvalState
-- kontState k =
--     case k of
--         KontTop0             -> Nothing
--         KontCallFun0 _ s     -> Just s
--         KontCallArg0 _ _ _ s -> Just s
--         KontIf0 _ _ s        -> Just s
--         KontThrowFun0 _ s    -> Just s
--         KontThrowArg0  _ s   -> Just s

-- shiftKont0 :: EvalC m => m ()
-- shiftKont0 = do
--     s0 <- get
--     case kontState (view (field @"esKont") s0) of
--         Nothing -> throwError EvalTopError
--         Just s1 -> put s1

-- addKont0 :: MonadState EvalState m => (EvalState -> Kont0) -> m ()
-- addKont0 f = do
--     s <- get
--     let k = f s
--     assign (field @"esKont") k

-- consumeKont0 :: EvalC m => (EvalState -> Kont0) -> m ()
-- consumeKont0 f = shiftKont0 >> addKont0 f

-- pushControl :: MonadState EvalState m => Name -> m ()
-- pushControl n = modify (\s@(EvalState k c m) -> EvalState k (c |> (n, s)) (M.insert n KontTerm m))

-- seqFindR :: Eq a => a -> Seq (a, b) -> Maybe b
-- seqFindR a abs = do
--     i <- Seq.findIndexR (\(x, _) -> x == a) abs
--     ab <- Seq.lookup i abs
--     pure (snd ab)

-- popControl :: EvalC m => Name -> m ()
-- popControl n = do
--     st <- use (field @"esStack")
--     case seqFindR n st of
--         Nothing -> throwError (EvalMissingContError n)
--         Just s  -> put s

-- step0 :: EvalC m => Exp0 Name -> m (Maybe (Exp0 Name))
-- step0 e =
--     case e of
--         Var0 n -> do
--             tmMap <- use (field @"esTmMap")
--             case M.lookup n tmMap of
--                 Nothing -> pure (Nothing)
--                 Just et ->
--                     case et of
--                         ExpTerm ee -> pure (Just ee)
--                         KontTerm   -> kstep0 e
--         Call0 e xs -> do
--             addKont0 (KontCallFun0 xs)
--             pure (Just e)
--         If0 g t e -> do
--             addKont0 (KontIf0 t e)
--             pure (Just g)
--         CallCC0 n _ b -> do
--             pushControl n
--             pure (Just (instantiate1 (Var0 n) b))
--         Throw0 c e -> do
--             addKont0 (KontThrowFun0 e)
--             pure (Just c)
--         _ -> kstep0 e

-- kstep0 :: EvalC m => Exp0 Name -> m (Maybe (Exp0 Name))
-- kstep0 e = do
--     k <- use (field @"esKont")
--     case k of
--         KontTop0 -> pure Nothing
--         KontCallFun0 xs _ ->
--             case e of
--                 Lam0 nts b ->
--                     case xs of
--                         Seq.Empty -> call0 (fst <$> nts) Seq.empty b
--                         x :<| xs -> do
--                             consumeKont0 (KontCallArg0 e Seq.empty xs)
--                             pure (Just x)
--                 _ -> throwError EvalNotLambdaError
--         KontCallArg0 fun ready notReady _ ->
--             let ready' = ready |> e
--             in case notReady of
--                 Seq.Empty -> do
--                     case fun of
--                         Lam0 nts b -> call0 (fst <$> nts) ready' b
--                         _          -> throwError EvalNotLambdaError
--                 x :<| xs -> do
--                     consumeKont0 (KontCallArg0 fun ready' xs)
--                     pure (Just x)
--         KontIf0 xt xe _ ->
--             case e of
--                 Bool0 b -> do
--                     shiftKont0
--                     pure (Just (if b then xt else xe))
--                 _ -> throwError EvalNotBoolError
--         KontThrowFun0 y _ -> do
--             consumeKont0 (KontThrowArg0 e)
--             pure (Just y)
--         KontThrowArg0 x _ ->
--             case x of
--                 Var0 n -> do
--                     popControl n
--                     pure (Just e)
--                 _ -> throwError EvalNotControlError

-- bigStep0 :: EvalC m => Exp0 Name -> m (Seq (Exp0 Name, EvalState), Maybe EvalError)
-- bigStep0 e =
--     let go w trail = do
--             x <- catchError (Right <$> step0 w) (pure . Left)
--             case x of
--                 Left e -> pure (trail, Just e)
--                 Right a ->
--                     case a of
--                         Nothing -> pure (trail, Nothing)
--                         Just y  -> do
--                             n <- get
--                             go y (trail |> (y, n))
--     in do
--         n <- get
--         go e (Seq.singleton (e, n))
