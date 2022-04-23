{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hw8 where

import Data.Tree (Tree (Node), rootLabel, subForest)
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons employee (GL employees fun) = GL (employee : employees) (fun + empFun employee)

instance Semigroup GuestList where
  (<>) (GL emp1 fun1) (GL emp2 fun2) = GL (emp1 ++ emp2) (fun1 + fun2)

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun g1 g2 = if g1 < g2 then g2 else g1

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f t = f (rootLabel t) (map (treeFold f) (subForest t))

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e list =
  ( glCons e (foldl (<>) mempty (map snd list)),
    foldl (<>) mempty (map fst list)
  )

maxFun :: Tree Employee -> GuestList
maxFun t = uncurry moreFun (treeFold nextLevel t)

guestListToString :: GuestList -> String
guestListToString (GL xs fun) = "Fun score:" ++ show fun ++ "\n" ++ unlines (map empName xs)

main :: IO()
main = readFile "data/company.txt" >>= \s -> putStr (guestListToString (maxFun (read s)))
