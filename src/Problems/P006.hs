module Problems.P006 where


{--

The sum of the squares of the first ten natural numbers is,
12 + 22 + ... + 102 = 385

The square of the sum of the first ten natural numbers is,
(1 + 2 + ... + 10)2 = 552 = 3025

Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.

Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

--}


--used the standard formula for both

p006 :: Integer
p006 = p006' 100
    where p006' :: Integer -> Integer
          p006' x = sqOsu - suOsq
              where sqOsu = (x*x * (x+1)* (x+1))   `div` 4
                    suOsq = (x * (x+1) * (2*x +1)) `div` 6  
