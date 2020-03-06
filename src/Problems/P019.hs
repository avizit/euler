module Problems.P019 where



  --
  -- You are given the following information, but you may prefer to do some research for yourself.
  --
  --     1 Jan 1900 was a Monday.
  --     Thirty days has September,
  --     April, June and November.
  --     All the rest have thirty-one,
  --     Saving February alone,
  --     Which has twenty-eight, rain or shine.
  --     And on leap years, twenty-nine.
  --     A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
  --
  -- How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

import System.IO
--import Data.Dates

data MyDay = Sunday | Monday | Tuesday | Wednessday | Thursday | Friday | Saturday
              deriving (Eq)
data MyDate = MyDate
  { day :: Int
  , month :: Int
  , year :: Int
  } deriving (Eq, Show)

class MyIncrementable t where
  inc :: t -> t

instance MyIncrementable MyDay where
  inc Sunday = Monday
  inc Monday = Tuesday
  inc Tuesday = Wednessday
  inc Wednessday = Thursday
  inc Thursday = Friday
  inc Friday = Saturday
  inc Saturday = Sunday


instance MyIncrementable MyDate where
  inc (MyDate dd mm yy)
    | dd == 31 && mm == 12  = MyDate 1 1 (yy +1)
    | dd == 31 = MyDate 1 (mm + 1) yy
    | dd == 30 && mm `elem` [4,6,9,11] = MyDate 1 (mm + 1) yy
    | dd == 29 && mm == 2 = MyDate 1 3 yy
    | dd == 28 && mm == 2 && (not $ isLeap yy) = MyDate 1 3 yy
    | otherwise = MyDate (dd +1) mm  yy

instance Ord MyDate where
  compare (MyDate d1 m1 y1)   (MyDate d2 m2 y2)
    | y1 /= y2 = compare  y1 y2
    | m1 /= m2 = compare m1 m2
    | d1 /= d2 = compare d1 d2
    | otherwise = EQ




isLeap :: Int -> Bool
isLeap y
  | (rem y 100 == 0) && (rem y 400 /= 0) = False
  | rem y 4 /= 0 = False
  | otherwise = True


knownDate = MyDate 1 1 1900  --Monday 
startDate = MyDate 1 1 1901
endDate   = MyDate 31 12 2000

numSundaysOnFirst :: Int
numSundaysOnFirst  = nsof 0 Monday knownDate
                      where nsof :: Int -> MyDay -> MyDate -> Int
                            nsof currCount currDay currDate
                              | currDate > endDate = currCount
                              | currDate < startDate = nsof currCount (inc currDay) (inc currDate)
                              | otherwise = if currDay == Sunday  && day currDate == 1
                                            then nsof (currCount +1) (inc currDay) (inc currDate)
                                            else nsof currCount  (inc currDay) (inc currDate)


p019:: Int
p019 = numSundaysOnFirst
