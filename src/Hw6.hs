{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Hw6 where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

fibs2 :: [Integer]
fibs2 = map fst (iterate (\(x, y) -> (y, x + y)) (0, 1))

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show s = show (take 20 (streamToList s))

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

maxPow :: Integer -> Integer
maxPow n
  | n <= 0 = 0
  | even n = maxPow (div n 2) + 1
  | otherwise = 0

ruler :: Stream Integer
ruler = streamMap (maxPow . (+ 1)) nats

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  negate = streamMap (0 -)
  fromInteger n = Cons n (streamRepeat 0)
  (Cons a as) + (Cons b bs) = Cons (a + b) (as + bs)
  (Cons a as) * p2@(Cons b bs) = Cons (a * b) (fromInteger a * bs + as * p2)

instance Fractional (Stream Integer) where
  p1@(Cons a as) / p2@(Cons b bs) =
    Cons
      (div a b)
      ((as - p1 / p2 * bs) / fromInteger b)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

