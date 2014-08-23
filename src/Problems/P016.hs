module Problems.P016 where
import Data.Char

p016 :: Int
p016 = sum $  ( map digitToInt ) $   show (2^1000)
