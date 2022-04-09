module TestHw1 where
import Hw1

prop_sumDigits :: Integer -> Bool
prop_sumDigits x = sumDigits (toDigits x) == sumDigits [x]
prop_doubleEveryOther :: [Integer] -> Bool
prop_doubleEveryOther x = 0:0:doubleEveryOther x == doubleEveryOther (0:0:x)