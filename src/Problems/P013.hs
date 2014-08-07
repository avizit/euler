module Problems.P013 where


p013 :: IO String
p013 = do
  input <- readFile "data/13.txt"
  let inputarr = (map read  $ lines input) :: [Integer]
      mysum:: Integer
      mysum = sum inputarr
    in return $ take 10 $ show mysum
       
