module Hw1 where

toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0 = []
  | otherwise = mod x 10 : toDigits (div x 10)

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

doubleFirst, doubleSecond, doubleEveryOther :: [Integer] -> [Integer]
doubleFirst [] = []
doubleFirst (x : xs) = x * 2 : doubleSecond xs

doubleSecond [] = []
doubleSecond (x : xs) = x : doubleFirst xs

doubleEveryOther list
  | mod (length list) 2 == 1 = doubleSecond list
  | otherwise = doubleFirst list

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : xs) = sum (toDigits x) + sumDigits xs

validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther (toDigitsRev x)) `mod` 10 == 0

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b _ = [(a, b)]
hanoi n a b c = hanoi (n - 1) a c b ++ (a, b) : hanoi (n - 1) c b a
