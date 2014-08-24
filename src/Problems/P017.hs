module Problems.P017 where

numberToWords :: Integer -> String
numberToWords 0 = "zero"
numberToWords 1 = "one"
numberToWords 2 = "two"
numberToWords 3 = "three"
numberToWords 4 = "four"
numberToWords 5 = "five"
numberToWords 6 = "six"
numberToWords 7 = "seven"
numberToWords 8 = "eight"
numberToWords 9 = "nine"

numberToWords 10 = "ten"
numberToWords 11 = "eleven"
numberToWords 12 = "twelve"
numberToWords 13 = "thirteen"
numberToWords 14 = "fourteen"
numberToWords 15 = "fifteen"
numberToWords 16 = "sixteen"
numberToWords 17 = "seventeen"
numberToWords 18 = "eighteen"
numberToWords 19 = "nineteen"

numberToWords 20 = "twenty"
numberToWords 30 = "thirty"
numberToWords 40 = "forty"
numberToWords 50 = "fifty"
numberToWords 60 = "sixty"
numberToWords 70 = "seventy"
numberToWords 80 = "eighty"
numberToWords 90 = "ninety"

numberToWords x  = thousandsStr ++ " " ++ hundredsStr ++ " " ++ maybeAnd ++ " " ++ rest
    where thousandsStr = if thousands x > 0
                         then ( numberToWords $ thousands x ) ++ " " ++ "thousand"
                         else ""
          hundredsStr  = if hundreds x > 0
                         then (numberToWords $ hundreds x ) ++ " " ++ "hundred"
                         else ""
          
          maybeAnd = if (x > 99)  && (x `mod` 100 > 0 ) 
                     then "and"
                     else ""
          rest = if (x `mod` 100 > 0)  && (x `mod` 100 < 20 )
                 then numberToWords $ x `mod` 100
                 else tensStr ++ " " ++ onesStr

          tensStr = if tens x > 0
                   then (numberToWords $ 10 * tens x )
                   else ""
          onesStr = if x `mod` 10 > 0
                    then numberToWords $ x `mod` 10
                    else ""

thousands x = ( x `mod` 10000 - x `mod` 1000 ) `div` 1000
hundreds  x = ( x `mod` 1000  - x `mod` 100  ) `div` 100
tens      x = ( x `mod` 100   - x `mod` 10   ) `div` 10
ones      x = x `mod` 10


--p017 :: Integer
p017 = foldr (+)   0 $ map f [1..1000]
       where f x =  sum $ map length $ words ( numberToWords x) 
