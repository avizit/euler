module Problems.P018 where

--import System.IO

f :: [Int] -> [Int] -> [Int] --the folding function
f [] ys = []
f (x:xs) (y:y1:ys) = max (x +y) ( x+y1 ) : f xs (y1:ys)



p018 ::IO ()
p018 = do
  indata <- readFile "data/p18.dat"
  putStrLn $ show $ maximum $ foldl1 f (read indata::[[Int]] ) 

