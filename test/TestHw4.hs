module TestHw4 where

import Hw4

propFun1 :: [Integer] -> Bool
propFun1 x = fun1 x == fun1o x

propFun2 :: Integer -> Bool
propFun2 x =
  let v = abs x + 1
   in fun2 v == fun2o v

propMap' :: (Integer -> Integer) -> [Integer] -> Bool
propMap' f n = map f n == map' f n

propMyFoldl :: (Integer -> String -> Integer) -> Integer -> [String] -> Bool
propMyFoldl f n l = foldl f n l == myFoldl f n l
