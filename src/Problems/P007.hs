module Problems.P007 where

import Utils.Numeric


{--By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.

What is the 10 001st prime number?
--}


p007 :: Integer
p007 = erosthenes !! 10000  --zero based array

