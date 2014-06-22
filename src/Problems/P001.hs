module Problems.P001 where


{--
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
--}

p001 :: Integer
p001 = sum $ filter divisableBy3or5 [1..1000]
    where divisableBy3or5 x = (x `mod` 3 == 0) || (x `mod` 5 == 0)  --to use || and not or 
