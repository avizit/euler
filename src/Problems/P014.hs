module Problems.P014 where

import qualified Data.Map as Map
import Control.Monad.Trans.State
import Control.Monad.Identity
import Control.Monad
import Control.Monad.Trans.Class 



twoPowersCollatz :: [(Integer, Integer)]
twoPowersCollatz = zipWith (,)  ( takeWhile (<1000000) ( iterate (*2) 1 )) [1..]


initCollatzMap :: Map.Map Integer Integer
initCollatzMap = Map.fromList twoPowersCollatz


--the following version can be improved by inserting into map - all numbers for x , 2*x , 4*x ..once we know the value for x
getCollatz :: Integer -> StateT (Map.Map Integer Integer) Identity Integer
getCollatz x = do
  mymap <- get
  case Map.lookup x mymap of
    Just val -> return val
    Nothing -> if (even x )
               then do
                 let  (aa, ss)  = (runState  $  getCollatz (x `div` 2)) mymap  
                 put ( Map.insert x (aa +1) ss )
                 return (aa +1) 
               else do
                 let  (aa,ss )  = (runState  $  getCollatz (3*x +1)) mymap
                 put ( Map.insert x (aa +1) ss )
                 return (aa +1)

--totally unnecessay data definitions  just to use the state monad and make it more readable . can definitely be done simpler by passing a hash or even an aarray  as the accumulator of foldr 
data NumAndCollatz = NumAndCollatz { number :: Integer
                                   , collatz :: Integer 
                                   } deriving (Show)
instance Eq NumAndCollatz where
    NumAndCollatz n1 c1 == NumAndCollatz n2 c2 = c1 == c2

 
instance Ord NumAndCollatz where
    NumAndCollatz n1 c1 `compare` NumAndCollatz n2 c2 = c1 `compare` c2


solveP014 :: NumAndCollatz
solveP014 = fst $ foldr f (NumAndCollatz 1 1 ,initCollatzMap) [1..1000000]
    where f :: Integer -> (NumAndCollatz  , Map.Map Integer Integer) -> (NumAndCollatz , Map.Map Integer Integer) 
          f x (a,b)  = (a' , b')
              where
                (cc, b') = (runState (getCollatz x)) b
                a' :: NumAndCollatz
                a' = max a  (NumAndCollatz x cc)



--main = putStrLn $ show solveP014

