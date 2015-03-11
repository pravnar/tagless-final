module DeBruinUT where

import Prelude hiding (lookup)
    
data E = V Int
       | N Int
       | B Bool
       | Leq E E
       | Plus E E
       | Times E E
       | If E E E
       | Lam E
       | App E E
       | Fix E
         deriving (Show, Eq)

-- | Interpretation into integer, boolean, and function types

data U = UN Int | UB Bool | UF (U -> U)

type Env = [U]

lookup :: Env -> Int -> U
lookup (x:_)  0 = x
lookup (_:xs) i = lookup xs (i-1)

eval :: Env -> E -> U
eval env exp =
    case exp of
        V i -> lookup env i
        N i -> UN i
        B b -> UB b
        Leq e1 e2   -> let (UN n1, UN n2) = (eval env e1, eval env e2)
                       in UB (n1 <= n2)
        Plus e1 e2  -> let (UN n1, UN n2) = (eval env e1, eval env e2)
                       in UN (n1 + n2)
        Times e1 e2 -> let (UN n1, UN n2) = (eval env e1, eval env e2)
                       in UN (n1 * n2)
        If ec et ef -> let UB b = eval env ec in
                       if b then eval env et else eval env ef
        Lam e       -> UF (\u -> eval (u:env) e)
        App e1 e2   -> let UF f = eval env e1 in f (eval env e2)
        Fix e       -> let UF f = eval env e in f (eval env (Fix e))

-- | An automatic CPS-er, which itself is CPS-ed
cps :: E -> Int -> Int -> (E -> Int -> E) -> E
cps exp n m k =
    case exp of
      V i -> let i' = if i < n-1 then 2*i + 1 else i + n
             in k (V i') m 
      N i -> k (N i) m
      B b -> k (B b) m
      Leq e1 e2   -> cps e1 n m
                     (\v1 m1 -> cps e2 n m1
                                (\v2 m2 -> k (Leq v1 v2) m2))
      Plus e1 e2  -> cps e1 n m
                     (\v1 m1 -> cps e2 n m1
                                (\v2 m2 -> k (Plus v1 v2) m2))
      Times e1 e2 -> cps e1 n m
                     (\v1 m1 -> cps e2 n m1
                                (\v2 m2 -> k (Times v1 v2) m2))
      If ec et ef -> cps ec n m (\vc mc -> If vc (cps et n mc k) (cps ef n mc k))
      Lam e       -> k (Lam (Lam (cps e (n+1) m (\v _ -> (App (V 0) v))))) m
      App e1 e2   -> cps e1 n m
                     (\v1 m1 -> cps e2 n m1
                                (\v2 m2 ->
                                 let m' = m2 + 1
                                 in (App
                                     (App v1 v2)
                                     (Lam (incr m' (k (V (-m')) m'))))))
      Fix e       -> cps e n m (\v m' -> k (Fix v) m')

incr :: Int -> E -> E
incr m exp =
    case exp of
      V i -> V (i+m)
      N i -> N i
      B b -> B b
      Leq e1 e2 -> Leq (incr m e1) (incr m e2)
      Plus e1 e2 -> Plus (incr m e1) (incr m e2)
      Times e1 e2 -> Times (incr m e1) (incr m e2)
      If ec et ef -> If (incr m ec) (incr m et) (incr m ef)
      Lam e -> Lam e
      App e1 e2 -> App (incr m e1) (incr m e2)
      Fix e -> Fix (incr m e)

               

