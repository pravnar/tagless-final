module TaglessFinal where

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
         deriving Show

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

fact :: E
fact = Fix (Lam
            (Lam
             (If (Leq (V 0) (N 1))
                 (N 1)
                 (Times (V 0)
                        (App (V 1) (Plus (V 0) (N (-1))))))))

run :: IO ()
run = do
  let     -- \x -> (\b -> if b then (3+x) else (3*x))
      e = Lam (Lam (If (V 0) (Plus (N 3) (V 1)) (Times (N 3) (V 1))))
      a = App (App e (N 2)) (B True)
  print e
  print a
  let UN ans = eval [] a -- ^ ans = 5
      UN onetwenty = eval [] (App fact (N ans))
  print ans
  print onetwenty
       
factorial = \n -> if n <= 0 then 1 else n * (factorial (n-1))

factcps = \k n -> if n <=0 then (k 1) else factcps (\x -> k $ n * x) (n-1)
