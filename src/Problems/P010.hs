module Problems.P010 where

import Utils.Numeric
import Data.List

{--The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.

Find the sum of all the primes below two million.--}

--this takes too long 
--p010 :: Integer
--p010 = foldl' (+) 0  $ takeWhile (< 2000000) erosthenes


p010 :: Integer
p010 = foldl' (+) 0  $ filter isPrime [1..2000000]
