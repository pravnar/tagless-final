{-# LANGUAGE GADTs #-}

module DeBruijnT where

data Var a = Z | S (Var a)

data E vars a where
    V     :: Var a        -> E a a
    N     :: Int          -> E () Int
    B     :: Bool         -> E () Bool
    Leq   :: E v1 Int     -> E v2 Int -> E (v1,v2) Bool
    Plus  :: E v1 Int     -> E v2 Int -> E (v1,v2) Int
    Times :: E v1 Int     -> E v2 Int -> E (v1,v2) Int
    If    :: E v Bool     -> E v' a   -> E v' a
    Lam   :: (E va a -> E vb b) -> E (a,vb) (a -> b)
    App   :: E (a,vb) (a -> b)  -> E va a -> E vb b
    Fix   :: E vars a           -> E vars a

             
             
-- eval :: Env -> E a -> a
 
