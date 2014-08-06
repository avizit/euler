module Problems.P012 where

import Utils.Numeric

p012 :: Integer
p012 = head $ dropWhile (\x -> numFactors x <= 500 )  $ map (\x ->  (x * (x+1))  `div` 2 ) [1..]

