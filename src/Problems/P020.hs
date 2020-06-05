module Problems.P020 where



-- n! means n × (n − 1) × ... × 3 × 2 × 1

-- For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
-- and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

-- Find the sum of the digits in the number 100!



import System.IO
--import Data.Dates

sumOfDigits :: Integer -> Integer
sumOfDigits x = sod x 0
  where sod x tsum = if x < 10
                     then x + tsum
                     else sod (x `div` 10) (tsum + x `rem` 10)

p019:: Integer
p019 = sumOfDigits (product [1..100])
