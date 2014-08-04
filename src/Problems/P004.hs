module Problems.P004 where

import Utils.Numeric
import Control.Monad
import Control.Monad.Logic

p004 :: Integer
p004 = maximum [x*y | x <- [100..999], y <- [100..999] , isPalindrome (show (x*y))]


--I thought can be made faster by starting backward and fairly choosing the numbers using the logic monad
--BUT I was wrong . A look at testGenerate will show you why . Leaving the below as lesson learnt
-- ITS NOT POSSIBLE TO ALWAYS COMPARE TWO PAIRS , (a,b) and (c,d ) which will have higer product is not possible to tell in advance without actually multiplying 

choices :: MonadPlus m => [a] -> m a
choices = msum . map return 

candidates ::  Logic Integer
candidates = choices [999,998..100]

fairGenerate :: Logic Integer
fairGenerate = 
    candidates >>- \x ->
    candidates >>- \y ->
    guard (isPalindrome (show (x*y))) >> return (x*y)

testCandidate :: Logic Integer
testCandidate = choices [9,8..1]

testGenerate :: Logic (Integer, Integer)
testGenerate = testCandidate >>- \x ->
               testCandidate >>- \y ->
               return (x,y)
  

p004' :: Integer
p004' = head $ observeAll fairGenerate
