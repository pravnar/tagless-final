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

fact :: E
fact = Fix (Lam
            (Lam
             (If (Leq (V 0) (N 1))
                 (N 1)
                 (Times (V 0) (App (V 1) (Plus (V 0) (N (-1))))))))

runeval :: IO ()
runeval = do
  let     -- \x -> (\b -> if b then (3+x) else (3*x))
      e = Lam (Lam (If (V 0) (Plus (N 3) (V 1)) (Times (N 3) (V 1))))
      a = App (App e (N 2)) (B True)
  print e
  print a
  let UN ans = eval [] a -- ^ ans = 5
      UN onetwenty = eval [] (App fact (N ans))
  print ans
  print onetwenty

fix f = f (fix f)        
       
factorial = fix $ \rf n -> if n <= 0 then 1 else n * (rf (n-1))
factorialcps = fix $ \rf n k -> if n <= 0 then (k 1) else rf (n-1) (\x -> k $ n * x)

fib = \n -> if n <= 1 then 1 else (if n <= 2 then 1 else fib (n-1) + fib (n-2))
fibcps = \n k -> if n <= 1 then (k 1)
                 else (if n <= 2 then (k 1)
                       else fibcps (n-1) (\x1 -> fibcps (n-2) (\x2 -> k (x1 + x2))))

-- | An automatic CPS-er, which itself is CPS-ed
cps :: E -> Int -> (E -> E) -> E
cps exp n k =
    case exp of
      V i -> let i' = if i < n-1 then 2*i + 1 else i + n
             in k (V i')
      N i -> k (N i)
      B b -> k (B b)
      Leq e1 e2   -> cps e1 n
                     (\v1 -> cps e2 n
                           (\v2 -> k (Leq v1 v2)))
      Plus e1 e2  -> cps e1 n
                     (\v1 -> cps e2 n
                             (\v2 -> k (Plus v1 v2)))
      Times e1 e2 -> cps e1 n
                     (\v1 -> cps e2 n
                             (\v2 -> k (Times v1 v2)))
      If ec et ef -> cps ec n (\vc -> If vc (cps et n k) (cps ef n k))
      Lam e       -> k (Lam (Lam (cps e (n+1) (\v -> (App (V 0) v)))))
      App e1 e2   -> cps e1 n
                     (\v1 -> cps e2 n
                             (\v2 -> (App (App v1 v2) (Lam (incr (k (V (-1))))))))
      Fix e       -> cps e n (\v -> k (Fix v))

incr :: E -> E
incr exp =
    case exp of
      V i -> V (i+1)
      N i -> N i
      B b -> B b
      Leq e1 e2 -> Leq (incr e1) (incr e2)
      Plus e1 e2 -> Plus (incr e1) (incr e2)
      Times e1 e2 -> Times (incr e1) (incr e2)
      If ec et ef -> If (incr ec) (incr et) (incr ef)
      Lam e -> Lam (incr e)
      App e1 e2 -> App (incr e1) (incr e2)
      Fix e -> Fix (incr e)

               

