module Template.MultMachine where

import Utils

type MultState = (Int, Int, Int, Int) -- ^ (n, m, d, t)

{- |
>>> evalMult (2,3,0,0)
[(2,3,0,0),(2,2,2,0),(2,2,1,1),(2,2,0,2),(2,1,2,2),(2,1,1,3),(2,1,0,4),(2,0,2,4),(2,0,1,5),(2,0,0,6)]
>>> last $ evalMult (2,3,0,0)
(2,0,0,6)
>>> putStr $ layn $ map show $ evalMult (2,3,0,0)
   1)   (2,3,0,0)
   2)   (2,2,2,0)
   3)   (2,2,1,1)
   4)   (2,2,0,2)
   5)   (2,1,2,2)
   6)   (2,1,1,3)
   7)   (2,1,0,4)
   8)   (2,0,2,4)
   9)   (2,0,1,5)
  10)   (2,0,0,6)
-}
evalMult :: MultState -> [MultState]
evalMult state = if multFinal state
    then [state]
    else state : evalMult (stepMult state)

stepMult :: MultState -> MultState
stepMult (n, m, d, t)
    | d >  0    = (n, m,   d-1, t+1)
    | d == 0    = (n, m-1, n,   t  )
    | otherwise = error "impossible!"

multFinal :: MultState -> Bool
multFinal (_, 0, 0, _) = True
multFinal _            = False
