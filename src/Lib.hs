module Lib
    ( someFunc
    ) where

import Data.Ord (comparing)


import Data.List (sortBy)


someFunc :: IO ()
someFunc = putStrLn (show (fib 10))

fib :: Integer -> Integer
fib x
  | x <= 2 = 1
  | otherwise = fib(x-1) + fib(x-2)

evenList :: [Integer] -> [Integer]
evenList list = filter even list

--sortBy :: (a -> a -> Ordering) -> [a] -> [a]
--comparing :: (Ord a) => (b -> a) -> b -> b -> Ordering
--length :: t a -> Int
--compare :: a -> a -> Ordering
sortStrings :: [String] -> [String]
sortStrings = sortBy ((comparing length) <> compare)

