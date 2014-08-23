module Problems.P015 where

import Utils.Numeric 

--basicallly you can go down 20 times and go right 20 times.
--so basically you can put all the 20 Ds in a row and then put the remaining Rs into the 21 slots that are created
--problem to solve is put n stones in r buckets , with some buckets may be empty 

--using http://en.wikipedia.org/wiki/Stars_and_bars_%28combinatorics%29





p015 :: Integer
p015 =  nchoosek (20 + 21 -1 ) 20
