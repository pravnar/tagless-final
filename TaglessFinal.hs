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
                 (Times (V 0) (App (V 1) (Plus (V 0) (N (-1))))))))

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

fix f = f (fix f)        
       
factorial = fix $ \rf n -> if n <= 0 then 1 else n * (rf (n-1))
factorialcps = fix $ \rf n k -> if n <= 0 then (k 1) else rf (n-1) (\x -> k $ n * x)

fib = \n -> if n <= 1 then 1 else (if n <= 2 then 1 else fib (n-1) + fib (n-2))
fibcps = \n k -> if n <= 1 then (k 1)
                 else (if n <= 2 then (k 1)
                       else fibcps (n-1) (\x1 -> fibcps (n-2) (\x2 -> k (x1 + x2))))


-- | An automatic CPS-er, which itself is CPS-ed
-- The CPS-er creates code that takes the continuation as the *first* argument
cps :: E -> (E -> E) -> E
cps exp k =
    case exp of
      V i -> k (V i)
      N i -> k (N i)
      B b -> k (B b)
      Leq e1 e2 -> cps e1
                   (\v1 -> cps e2
                           (\v2 -> k (Leq v1 v2)))
      Plus e1 e2 -> cps e1
                    (\v1 -> cps e2
                            (\v2 -> k (Plus v1 v2)))
      Times e1 e2 -> cps e1
                     (\v1 -> cps e2
                             (\v2 -> k (Times v1 v2)))
      If ec et ef -> cps ec (\vc -> If vc (cps et k) (cps ef k))
      Lam e -> k (Lam (Lam (cps e
                            (\v -> (App (V 1) v)))))
      App e1 e2 -> cps e1
                   (\v1 -> cps e2
                           (\v2 -> (App (App v1 (Lam (k (V 0)))) v2)))
      Fix e -> cps e (\v -> k (Fix v))
               
-- | cps (App (Lam (V 0)) (B True)) with k = id
-- (App (App (Lam (Lam (App (V 1) (V 0)))) (Lam (k (V 0)))) (B True))
-- (App (Lam (App (Lam (k (V 0))) (V 0))) (B True))
-- (App (Lam (k (V 0))) (B True))
-- (k (B True))
-- (B True)


-- | For fact, we should get:

-- Fix (Lam -- k
--      (Lam -- rec
--       (Lam -- n
--        (If (Leq (V 0) (N 1))
--        (App (V 2) (N 1))
--        (App
--         (App (V 1) (Lam (App (V 3) (Times (V 1) (V 0)))))
--         (Plus (V 0) (N (-1))))))))


------ Let's step through what currently happens ------
-- cps almost-fact with k = id, where almost-fact is:

-- (Lam
--  (Lam
--   (If (Leq (V 0) (N 1))
--       (N 1)
--       (Times (V 0) (App (V 1) (Plus (V 0) (N (-1))))))))

-- (Lam
--  (Lam
--   (cps (Lam
--         (If (Leq (V 0) (N 1))
--             (N 1)
--             (Times (V 0) (App (V 1) (Plus (V 0) (N (-1)))))))
--        (\v -> (App (V 1) v)))))

-- (Lam
--  (Lam
--   (App (V 1)
--        (Lam
--         (Lam
--          (cps (If (Leq (V 0) (N 1))
--                   (N 1)
--                   (Times (V 0) (App (V 1) (Plus (V 0) (N (-1))))))
--               (\v -> (App (V 1) v))))))))

-- (Lam
--  (Lam
--   (App (V 1)
--    (Lam
--     (Lam
--      (cps (Leq (V 0) (N 1))
--       (\vc -> If vc
--               (cps (N 1)
--                (\v -> (App (V 1) v)))
--               (cps (Times (V 0) (App (V 1) (Plus (V 0) (N (-1)))))
--                (\v -> (App (V 1) v))))))))))

-- (Lam
--  (Lam
--   (App (V 1)
--    (Lam
--     (Lam
--      (cps (V 0)
--       (\v1 -> cps (N 1)
--               (\v2 -> ((\vc -> If vc
--                                (cps (N 1)
--                                 (\v -> (App (V 1) v)))
--                                (cps (Times (V 0) (App (V 1) (Plus (V 0) (N (-1)))))
--                                 (\v -> (App (V 1) v))))
--                        (Leq v1 v2))))))))))

-- (Lam
--  (Lam
--   (App (V 1)
--    (Lam
--     (Lam
--      (cps (N 1)
--       (\v2 -> ((\vc -> If vc
--                        (cps (N 1)
--                         (\v -> (App (V 1) v)))
--                        (cps (Times (V 0) (App (V 1) (Plus (V 0) (N (-1)))))
--                         (\v -> (App (V 1) v))))
--                (Leq (V 0) v2)))))))))

-- (Lam
--  (Lam
--   (App (V 1)
--    (Lam
--     (Lam
--      ((\vc -> If vc
--               (cps (N 1)
--                (\v -> (App (V 1) v)))
--               (cps (Times (V 0) (App (V 1) (Plus (V 0) (N (-1)))))
--                (\v -> (App (V 1) v))))
--       (Leq (V 0) (N 1))))))))

-- (Lam
--  (Lam
--   (App (V 1)
--    (Lam
--     (Lam
--      (If (Leq (V 0) (N 1))
--       (cps (N 1)
--        (\v -> (App (V 1) v)))
--       (cps (Times (V 0) (App (V 1) (Plus (V 0) (N (-1)))))
--        (\v -> (App (V 1) v)))))))))

-- (Lam
--  (Lam
--   (App (V 1)
--    (Lam
--     (Lam
--      (If (Leq (V 0) (N 1))
--       (App (V 1) (N 1))
--       (cps (V 0)
--        (\v1 -> cps (App (V 1) (Plus (V 0) (N (-1))))
--                (\v2 -> ((\v -> (App (V 1) v)) (Times v1 v2)))))))))))

-- (Lam
--  (Lam
--   (App (V 1)
--    (Lam
--     (Lam
--      (If (Leq (V 0) (N 1))
--       (App (V 1) (N 1))
--       (cps (App (V 1) (Plus (V 0) (N (-1))))
--        (\v2 -> ((\v -> (App (V 1) v)) (Times (V 0) v2))))))))))

-- (Lam
--  (Lam
--   (App (V 1)
--    (Lam
--     (Lam
--      (If (Leq (V 0) (N 1))
--       (App (V 1) (N 1))
--       (cps (V 1)
--        (\v1 -> cps (Plus (V 0) (N (-1)))
--                (\v2 -> (App (App v1 (Lam
--                                      ((\v2 -> ((\v -> (App (V 1) v)) (Times (V 0) v2)))
--                                       (V 0))))
--                        v2))))))))))

-- (Lam
--  (Lam
--   (App (V 1)
--    (Lam
--     (Lam
--      (If (Leq (V 0) (N 1))
--       (App (V 1) (N 1))
--       (cps (Plus (V 0) (N (-1)))
--        (\v2 -> (App (App (V 1) (Lam
--                                 ((\v2 -> ((\v -> (App (V 1) v)) (Times (V 0) v2)))
--                                  (V 0))))
--                 v2)))))))))

-- (Lam
--  (Lam
--   (App (V 1)
--    (Lam
--     (Lam
--      (If (Leq (V 0) (N 1))
--       (App (V 1) (N 1))
--       (cps (V 0)
--        (\v1 -> cps (N (-1))
--                (\v2 -> ((\v2 -> (App (App (V 1)
--                                       (Lam
--                                        ((\v2 -> ((\v -> (App (V 1) v))
--                                                  (Times (V 0) v2)))
--                                         (V 0))))
--                                  v2))
--                         (Plus v1 v2)))))))))))

-- (Lam
--  (Lam
--   (App (V 1)
--    (Lam
--     (Lam
--      (If (Leq (V 0) (N 1))
--       (App (V 1) (N 1))
--       (cps (N (-1))
--        (\v2 -> ((\v2 -> (App (App (V 1)
--                               (Lam
--                                ((\v2 -> ((\v -> (App (V 1) v))
--                                          (Times (V 0) v2)))
--                                 (V 0))))
--                          v2))
--                 (Plus (V 0) v2))))))))))

-- (Lam
--  (Lam
--   (App (V 1)
--    (Lam
--     (Lam
--      (If (Leq (V 0) (N 1))
--       (App (V 1) (N 1))
--       ((\v2 -> (App (App (V 1)
--                       (Lam
--                        ((\v2 -> ((\v -> (App (V 1) v))
--                                  (Times (V 0) v2)))
--                         (V 0))))
--                 v2))
--        (Plus (V 0) (N (-1))))))))))

-- (Lam
--  (Lam
--   (App (V 1)
--    (Lam
--     (Lam
--      (If (Leq (V 0) (N 1))
--       (App (V 1) (N 1))
--       (App
--        (App (V 1) (Lam
--                    ((\v2 -> ((\v -> (App (V 1) v))
--                              (Times (V 0) v2)))
--                     (V 0))))
--        (Plus (V 0) (N (-1))))))))))

-- (Lam
--  (Lam
--   (App (V 1)
--    (Lam
--     (Lam
--      (If (Leq (V 0) (N 1))
--       (App (V 1) (N 1))
--       (App (App (V 1) (Lam ((\v -> (App (V 1) v))
--                             (Times (V 0) (V 0)))))
--        (Plus (V 0) (N (-1))))))))))

-- (Lam
--  (Lam
--   (App (V 1)
--    (Lam
--     (Lam
--      (If (Leq (V 0) (N 1))
--       (App (V 1) (N 1))
--       (App
--        (App (V 1) (Lam (App (V 1) (Times (V 0) (V 0)))))
--        (Plus (V 0) (N (-1))))))))))

