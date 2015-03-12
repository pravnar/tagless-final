{-# LANGUAGE GADTs #-}

module DeBruijnT where

import Prelude hiding (lookup)

data Var a = Z | S (Var a)

data E a where
    C     :: E a
    V     :: Var a        -> E a
    N     :: Int          -> E Int
    B     :: Bool         -> E Bool
    Leq   :: E Int        -> E Int -> E Bool
    Plus  :: E Int        -> E Int -> E Int
    Times :: E Int        -> E Int -> E Int
    If    :: E Bool       -> E a -> E a -> E a
    Lam   :: (E a -> E b) -> E (a -> b)
    App   :: E (a -> b)   -> E a -> E b
    Fix   :: E (a -> a)   -> E a

data U where
    U :: a -> U

type Env = [U]

lookup :: Env -> Var a -> U
lookup []     _     = error "empty env"
lookup (x : _) Z  = x
lookup (_:xs) (S v) = lookup xs v
             
eval :: Env -> E a -> a
eval env exp =
    case exp of
      V v -> undefined -- lookup env v
      N i -> i
      B b -> b
      Leq e1 e2 -> eval env e1 <= eval env e2
      Plus e1 e2 -> eval env e1 + eval env e2
      Times e1 e2 -> eval env e1 * eval env e2
      If ec et ef -> if eval env ec then eval env et else eval env ef
      Lam f -> \u -> eval (U u : env) (incr 1 (f C))
      App e1 e2 -> eval env e1 $ eval env e2
      Fix f -> eval env f $ eval env (Fix f)

incr :: Int -> E a -> E a
incr = undefined               
 
