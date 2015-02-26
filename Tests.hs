module Tests where

import TaglessFinal hiding (fact)
       
cpstests :: [(E,E)]
cpstests = [ (t1, t1cps)
           , (t2, t2cps)
           , (fact, factcps)
           , (t3, t3cps)
           , (t4, t4cps) ]

testcps :: (E,E) -> IO ()           
testcps (t,t') = do
  let result = cps t 0 id
  if result == t' then return ()
  else (do putStrLn "Test failure "
           putStrLn "cps of "
           print $ show t
           putStrLn "should be"
           print $ show t1cps
           putStrLn "is actually"
           print $ show result)

runcpstests :: IO ()
runcpstests = mapM_ testcps cpstests >> putStrLn "Done."

t1 = (Lam (Lam (App (V 1) (V 0))))
t1cps = (Lam
         (Lam
          (App
           (V 0)
           (Lam
            (Lam
             (App
              (App (V 3) (V 1))
              (Lam (App (V 1) (V 0)))))))))

t2 = (Plus (If (B True) (N 2) (N 3)) (N 5))
t2cps = (If (B True)
         (Plus (N 2) (N 5))
         (Plus (N 3) (N 5)))
        
fact = Fix (Lam
            (Lam
             (If (Leq (V 0) (N 1))
                 (N 1)
                 (Times (V 0) (App (V 1) (Plus (V 0) (N (-1))))))))
factcps = Fix 
          (Lam 
           (Lam 
            (App (V 0) 
             (Lam 
              (Lam 
               (If (Leq (V 1) (N 1)) 
                (App (V 0) (N 1)) 
                (App 
                 (App (V 3) (Plus (V 1) (N (-1)))) 
                 (Lam (App (V 1) (Times (V 2) (V 0)))))))))))

t3 = (Plus (App (V 3) (N 10)) (N 50))
t3cps = (App
         (App (V 3) (N 10))
         (Lam (Plus (V 0) (N 50))))

t4 = (Plus (App (V 3) (N 10)) (V 50))
t4cps = (App
         (App (V 3) (N 10))
         (Lam (Plus (V 0) (V 51))))
          
