module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn (show (fib 10))

fib :: Integer -> Integer
fib x
  | x <= 2 = 1
  | otherwise = fib(x-1) + fib(x-2)

evenList :: [Integer] -> [Integer]
evenList list = filter even list
