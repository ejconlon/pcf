{-# LANGUAGE TemplateHaskell #-}

module Pcf.V3.JoinPoints where

import Bound (Scope, makeBound)
import Control.Applicative (empty)
import Control.Monad (ap)
import Control.Monad.Cont (ContT, runContT)
import Control.Monad.Trans (lift)
import Data.Deriving (deriveEq, deriveEq1, deriveShow, deriveShow1)
import Data.Map (Map)
import qualified Data.Map as M

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

type Name = String

data Type0 =
      TyBool0
    | TyArr0 [Type0] Type0
    | TyCont0 Type0
    deriving (Eq, Show)

data Exp0 a =
      Var0 a
    | Call0 (Exp0 a) [Exp0 a]
    | Lam0 [(Name, Type0)] (Scope Int Exp0 a)
    | Bool0 Bool
    | If0 (Exp0 a) (Exp0 a) (Exp0 a)
    | Control0 Name Type0 (Scope () Exp0 a)
    deriving (Functor, Foldable, Traversable)

instance Applicative Exp0 where
    pure = Var0
    (<*>) = ap

instance Monad Exp0 where
    return = pure
    (>>=) = undefined

$(deriveEq ''Exp0)
$(deriveShow ''Exp0)
$(deriveEq1 ''Exp0)
$(deriveShow1 ''Exp0)

-- $(makeBound ''Exp0)

data SuspType0 = CallSusp0 | JumpSusp0 deriving (Eq, Show)

data FunType0 = NonFun0 | CallFun0 | JumpFun0 | SuspFun0 SuspType0 Int deriving (Eq, Show)

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

checkType0 :: Map Name Type0 -> Type0 -> Exp0 Name -> Maybe ()
checkType0 = undefined

inferType0 :: Map Name Type0 -> Exp0 Name -> Maybe Type0
inferType0 m (Var0 n) = M.lookup n m
inferType0 m (Lam0 nts e) = undefined
inferType0 m (Call0 e xs) = do
    xts <- traverse (inferType0 m) xs
    et <- inferType0 m e
    undefined
inferType0 _ Bool0{} = pure TyBool0
inferType0 m (If0 g t e) = do
    checkType0 m TyBool0 g
    tt <- inferType0 m t
    checkType0 m tt e
    pure tt
inferType0 m (Control0 n t b) = do
    checkType0 (M.insert n (TyCont0 t) m) t undefined
    pure t


-- type BigStep0 = ContT (Exp0 Name) Maybe (Exp0 Name)

bigStep0 :: Map Name (Exp0 Name) -> Exp0 Name -> Maybe (Exp0 Name)
bigStep0 m (Var0 n) = M.lookup n m
bigStep0 _ e@Lam0{} = pure e
bigStep0 m (Call0 e xs) = do
    xs' <- traverse (bigStep0 m) xs
    e' <- bigStep0 m e
    case e' of
        Lam0 ns b -> undefined
        _ -> empty
bigStep0 _ e@Bool0{} = pure e
bigStep0 m (If0 g t e) = do
    g' <- bigStep0 m g
    case g' of
        Bool0 True -> bigStep0 m t
        Bool0 False -> bigStep0 m e
        _ -> empty
bigStep0 m (Control0 n t b) = undefined

-- runBigStep0 :: BigStep0 -> Maybe (Exp0 Name)
-- runBigStep0 = flip runContT pure

-- goOne and goAll would be represented

-- TODO actually bind variables
mkLam0 :: [(Name, Type0)] -> Exp0 a -> Exp0 a
mkLam0 ns e = Lam0 ns undefined

goOne0 :: Exp0 Name
goOne0 = mkLam0 [("a", TyBool0)] (Call0 (Var0 "xor") [Var0 "a"])

goAll0 :: Exp0 Name
goAll0 = mkLam0 [("a", TyBool0), ("b", TyBool0)] (Call0 (Var0 "xor") [Var0 "a", Var0 "b"])
