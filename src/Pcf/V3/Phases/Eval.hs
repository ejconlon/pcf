module Pcf.V3.Phases.Eval where

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
import           Data.Map              (Map)
import qualified Data.Map              as M
import           Data.Sequence         (Seq (..), (|>))
import qualified Data.Sequence         as Seq
import           Data.Text             (Text)
import qualified Data.Text             as T
import           GHC.Generics          (Generic)
import           Pcf.Core.BoundCrazy   (instantiateM)
import           Pcf.Core.Func         (FuncT)
import           Pcf.Core.Util         (filterMap, findL, insertAll, izipWithM_, localMod,
                                        lookupR, zipWithIndex)
import           Pcf.V3.Types

data Kont0 =
      KontTop0
    | KontCallFun0 (Seq (Exp0 Name)) EvalState
    | KontCallArg0 (Exp0 Name) (Seq (Int, Name, Exp0 Name)) Int Name (Seq (Int, Name, Exp0 Name)) EvalState
    | KontLet0 Ident (Scope () Exp0 Name) EvalState
    | KontCase0 (Seq (Pat0 Name)) EvalState
    | KontThrowFun0 (Exp0 Name) EvalState
    | KontThrowArg0 (Exp0 Name) EvalState
    | KontConArg0 Name (Seq (Exp0 Name)) (Seq (Exp0 Name)) EvalState
    | KontConReady0 EvalState
    deriving (Eq, Show)

data EvalError =
      EvalTopError
    | EvalUnboundVarError Int
    | EvalMissingContError Name
    | EvalLamArityError Int Int
    | EvalConArityError Name Int Int
    | EvalNotLambdaError
    | EvalNotControlError
    | EvalUnmatchedCaseError
    | EvalUnknownConError Name
    deriving (Generic, Eq, Show)

data EvalTerm =
      KontTerm
    | ExpTerm (Exp0 Name)
    deriving (Generic, Eq, Show)

data EvalState = EvalState
    { esKont  :: Kont0
    , esStack :: Seq (Name, EvalState)
    , esTmMap :: Map Name EvalTerm
    } deriving (Generic, Eq, Show)

data EvalEnv t = EvalEnv
    { eeDataDefs :: DataDefs t
    } deriving (Generic, Eq, Show)

type EvalC t m = (MonadReader (EvalEnv t) m, MonadState EvalState m, MonadError EvalError m)
type EvalT t m a = FuncT (EvalEnv t) EvalState EvalError m a

evalProof :: Monad m => (forall n. EvalC t n => n a) -> EvalT t m a
evalProof = id

looking :: (MonadError EvalError m, Eq b) => (b -> EvalError) -> Seq (b, Name, a) -> (b -> m (Exp0 Name))
looking e inxs i =
    case findL (\(j, n, _) -> if (i == j) then Just n else Nothing) inxs of
        Nothing -> throwError (e i)
        Just n  -> pure (Var0 n)

insertAllExps :: EvalC t m => Seq (b, Name, Exp0 Name) -> m ()
insertAllExps inxs = modifying (field @"esTmMap") (insertAll ((\(_, n, x) -> (n, ExpTerm x)) <$> inxs))

validateArity :: EvalC t m => (Int -> Int -> m ()) -> Seq a -> Seq b -> m ()
validateArity e xs as =
    let xlen = Seq.length xs
        alen = Seq.length as
    in if xlen /= alen
        then e xlen alen
        else pure ()

inst0 :: (EvalC t m, Eq b) => (b -> EvalError) -> Seq (b, Name, Exp0 Name) -> Scope b Exp0 Name -> m (Maybe (Exp0 Name))
inst0 e inxs b = do
    shiftKont0
    insertAllExps inxs
    Just <$> instantiateM (looking e inxs) b

call0 :: EvalC t m => Seq (Int, Name, Exp0 Name) -> Scope Int Exp0 Name -> m (Maybe (Exp0 Name))
call0 = inst0 EvalUnboundVarError

kontState :: Kont0 -> Maybe EvalState
kontState k =
    case k of
        KontTop0                 -> Nothing
        KontCallFun0 _ s         -> Just s
        KontCallArg0 _ _ _ _ _ s -> Just s
        KontCase0 _ s            -> Just s
        KontThrowFun0 _ s        -> Just s
        KontThrowArg0 _ s        -> Just s
        KontConArg0 _ _ _ s      -> Just s
        KontConReady0 s          -> Just s
        KontLet0 _ _ s           -> Just s

useKontState :: MonadState EvalState m => m (Maybe EvalState)
useKontState = kontState <$> use (field @"esKont")

shiftKont0 :: EvalC t m => m ()
shiftKont0 = useKontState >>= maybe (throwError EvalTopError) put

addKont0 :: MonadState EvalState m => (EvalState -> Kont0) -> m ()
addKont0 f = do
    s <- get
    let k = f s
    assign (field @"esKont") k

consumeKont0 :: EvalC t m => (EvalState -> Kont0) -> m ()
consumeKont0 f = shiftKont0 >> addKont0 f

pushControl :: MonadState EvalState m => Name -> m ()
pushControl n = modify (\s@(EvalState k c m) -> EvalState k (c |> (n, s)) (M.insert n KontTerm m))

popControl :: EvalC t m => Name -> m ()
popControl n = do
    st <- use (field @"esStack")
    case lookupR n st of
        Nothing -> throwError (EvalMissingContError n)
        Just s  -> put s

selectPat :: EvalC t m => Exp0 Name -> Seq (Pat0 Name) -> m (Exp0 Name, Seq (Int, Name, Exp0 Name))
selectPat e ps =
    case ps of
        p :<| ps' ->
            case p of
                VarPat0 n b -> pure (instantiate1 e b, Seq.empty)
                WildPat0 e' -> pure (e', Seq.empty)
                ConPat0 n is b ->
                    case e of
                        Con0 n' xs | n == n' -> do
                            let inxs = filterMap nameFilter3 (zipWithIndex is xs)
                            e <- instantiateM (looking (const (EvalUnboundVarError 0)) inxs) b
                            pure (e, inxs)
                        _ -> selectPat e ps'
        Empty -> throwError EvalUnmatchedCaseError

step0 :: EvalC t m => Exp0 Name -> m (Maybe (Exp0 Name))
step0 e =
    case e of
        Var0 n -> do
            tmMap <- use (field @"esTmMap")
            case M.lookup n tmMap of
                Nothing -> pure (Nothing)
                Just et ->
                    case et of
                        ExpTerm ee -> pure (Just ee)
                        KontTerm   -> kstep0 e
        Call0 e xs -> do
            addKont0 (KontCallFun0 xs)
            pure (Just e)
        Case0 e ps -> do
            addKont0 (KontCase0 ps)
            pure (Just e)
        CallCC0 n _ b -> do
            pushControl n
            pure (Just (instantiate1 (Var0 n) b))
        Throw0 c e -> do
            addKont0 (KontThrowFun0 e)
            pure (Just c)
        Let0 i e b -> do
            addKont0 (KontLet0 i b)
            pure (Just e)
        The0 e _ -> do
            pure (Just e)
        Con0 n xs -> do
            k <- use (field @"esKont")
            case k of
                KontConReady0 _ -> shiftKont0 >> kstep0 e
                _ -> case xs of
                    y :<| ys -> do
                        addKont0 (KontConArg0 n Seq.empty ys)
                        pure (Just y)
                    Empty -> kstep0 e
        _ -> kstep0 e

kstep0 :: EvalC t m => Exp0 Name -> m (Maybe (Exp0 Name))
kstep0 e = do
    k <- use (field @"esKont")
    case k of
        KontTop0 -> pure Nothing
        KontCallFun0 xs _ -> do
            case e of
                Lam0 its b -> do
                    validateArity (\i j -> throwError (EvalLamArityError i j)) xs its
                    let inxs = filterMap nameFilter3 (zipWithIndex (fst <$> its) xs)
                    case inxs of
                        Seq.Empty -> call0 Seq.empty b
                        (i, n, w) :<| ys -> do
                            consumeKont0 (KontCallArg0 e Seq.empty i n ys)
                            pure (Just w)
                _ -> throwError EvalNotLambdaError
        KontCallArg0 fun ready i n notReady _ ->
            let ready' = ready |> (i, n, e)
            in case notReady of
                Seq.Empty -> do
                    case fun of
                        Lam0 its b -> call0 ready' b
                        _          -> throwError EvalNotLambdaError
                (j, m, w) :<| ys -> do
                    consumeKont0 (KontCallArg0 fun ready' j m ys)
                    pure (Just w)
        KontCase0 ps _ -> do
            (e', inxs) <- selectPat e ps
            shiftKont0
            insertAllExps inxs
            pure (Just e')
        KontThrowFun0 y _ -> do
            consumeKont0 (KontThrowArg0 e)
            pure (Just y)
        KontThrowArg0 x _ ->
            case x of
                Var0 n -> do
                    popControl n
                    pure (Just e)
                _ -> throwError EvalNotControlError
        KontLet0 i b _ ->
            let inxs = case i of
                    ConcreteIdent n -> Seq.singleton ((), n, e)
                    WildIdent       -> Seq.empty
            in inst0 (const (EvalUnboundVarError 0)) inxs b
        KontConArg0 n ready notReady s ->
            let ready' = ready |> e
            in case notReady of
                y :<| ys -> do
                    consumeKont0 (KontConArg0 n ready' ys)
                    pure (Just y)
                Empty -> do
                    consumeKont0 KontConReady0
                    pure (Just (Con0 n ready'))

traceBigStep0 :: EvalC t m => Exp0 Name -> m (Seq (Exp0 Name, EvalState), Either EvalError (Exp0 Name))
traceBigStep0 e =
    let go w trail = do
            x <- catchError (Right <$> step0 w) (pure . Left)
            case x of
                Left e -> pure (trail, Left e)
                Right a ->
                    case a of
                        Nothing -> pure (trail, Right w)
                        Just y  -> do
                            n <- get
                            go y (trail |> (y, n))
    in do
        n <- get
        go e (Seq.singleton (e, n))

bigStep0 :: EvalC t m => Exp0 Name -> m (Either EvalError (Exp0 Name))
bigStep0 w = do
    x <- catchError (Right <$> step0 w) (pure . Left)
    case x of
        Left e -> pure (Left e)
        Right a ->
            case a of
                Nothing -> pure (Right w)
                Just w' -> bigStep0 w'
