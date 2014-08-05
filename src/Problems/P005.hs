module Problems.P005 where

import Utils.Numeric

{-- 

2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.

What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
--}

p005 :: Integer
p005 = foldr my_lcm 1 [1..20]
