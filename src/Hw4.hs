module Hw4 where

import Data.List (sort, (\\))

fun1o :: [Integer] -> Integer
fun1o [] = 1
fun1o (x : xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2o :: Integer -> Integer
fun2o 1 = 0
fun2o n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun1 :: [Integer] -> Integer
fun1 list = foldr (\a b -> (a - 2) * b) 1 (filter even list)

fun2 :: Integer -> Integer
fun2 n =
  sum
    ( filter
        even
        ( takeWhile
            (/= 1)
            ( iterate
                ( \x ->
                    if even x
                      then div x 2
                      else 3 * x + 1
                )
                n
            )
        )
    )

data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node _ l val r) =
  if height l < height r
    then Node (height r + 1) (insert x l) val r
    else
      let newNode = insert x r
       in Node (max (height l) (height newNode) + 1) l val newNode

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x l -> f x : l) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z list = foldr (flip f) z (reverse list)

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
  let composites = map (\(x, y) -> x + y + 2 * x * y) (filter (uncurry (<=)) (cartProd [1 .. n] [1 .. n]))
   in map (\x -> x * 2 + 1) ([1 .. n] \\ composites)
