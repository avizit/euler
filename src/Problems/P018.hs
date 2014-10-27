module Problems.P018 where

--import System.IO

f :: [Int] -> [Int] -> [Int]  --bigger array first
f xs [] = xs
f (x1:x2:xs) (y1:ys) = (y1 + max x1 x2) : f (x2:xs) ys


p018 ::IO ()
p018 = do
  indata <- readFile "data/p18.dat"
  putStrLn indata
  putStrLn $ show $ maximum $ foldl1 f $ reverse (map (map read) $  map words $ lines indata   )
--    where
--      myarr :: [[Int]]
--      myarr =  map (map read) $  map words $ lines indata
    

