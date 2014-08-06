module Utils.Numeric where


import Test.QuickCheck
import Data.List

myfib :: [Integer]
myfib = 1:2:(zipWith (+) myfib (tail myfib))

factorise :: Integer -> [Integer]
factorise x | x <= 1 = [x]
            | otherwise = f' [1] 2 x
  where f':: [Integer] -> Integer -> Integer -> [Integer]
        f' res curr currX = if curr > sqrtx
                            then currX:res
                            else if currX `mod` curr == 0
                                 then f' (curr:res) curr     (currX `div` curr)
                                 else f' res        (curr+1) currX
        sqrtx = floor (sqrt (fromIntegral x))

prop_factprod :: Integer -> Bool
prop_factprod x = x == product ( factorise x )

isPalindrome :: String -> Bool
isPalindrome x = x == reverse x

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime x = null $ filter (\y -> x `mod` y == 0 ) [2..z]
  where z = floor (sqrt (fromIntegral x))


erosthenes :: [Integer]
erosthenes = ert [2..]
    where ert (x:xs) = x : ert ( filter (\y -> y `mod` x /= 0 ) xs )
              


factors :: Integer -> [Integer]
factors x = filter (\y -> x `mod` y == 0 )  [1..x]

primeFactorise :: Integer -> [Integer]
primeFactorise x = f' [] 2 x
    where f' :: [Integer] -> Integer -> Integer -> [Integer]
          f' res curr currX |  curr > sqrtx  =   res ++ [currX]
                            |  curr == currX =   res ++ [currX]
                            |  currX `mod` curr == 0 = f' (res ++ [curr]) curr (currX `div` curr )
                            |  otherwise   =  f' res (curr +1 )  currX
          sqrtx = floor (sqrt (fromIntegral x))


numFactors :: Integer -> Int --because length returns int
numFactors  x =  product $ map (\y -> y+1) $ map length $ groupBy (==) (primeFactorise x )



my_gcd :: Integral a => a -> a -> a
my_gcd x y | x < y = my_gcd y x
        | otherwise = if x `mod` y == 0
                      then y
                      else my_gcd y ( x `mod` y)

my_lcm :: Integral a => a -> a -> a
my_lcm x y = x * y `div` (my_gcd x y )
