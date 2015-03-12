{-# LANGUAGE GADTs #-}

module DeBruijnT where

data Var a = Z | S (Var a)

data E a where
    V     :: Var a        -> E a
    N     :: Int          -> E Int
    B     :: Bool         -> E Bool
    Leq   :: E Int        -> E Int -> E Bool
    Plus  :: E Int        -> E Int -> E Int
    Times :: E Int        -> E Int -> E Int
    If    :: E Bool       -> E a   -> E a
    Lam   :: (E a -> E b) -> E (a -> b)
    App   :: E (a -> b)   -> E a -> E b
    Fix   :: E a          -> E a
             
-- eval :: Env -> E a -> a
 
