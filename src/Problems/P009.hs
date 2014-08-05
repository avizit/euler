module Problems.P009 where
{--

A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
a2 + b2 = c2

For example, 32 + 42 = 9 + 16 = 25 = 52.

There exists exactly one Pythagorean triplet for which a + b + c = 1000.
Find the product abc.

--}


p009 :: Integer
p009 = head [a*b*c | a <- [1..500],b <-[1..500], c <-[1..500], b < a , a < c , a + b + c == 1000, a + b > c , a + b >= 500, a*a + b*b == c*c ]
