{-# LANGUAGE FlexibleInstances #-}

module Hw5 where
import StackVM
import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit x) = x
eval (ExprT.Add x y) = eval x + eval y
eval (ExprT.Mul x y) = eval x * eval y

evalStr :: String -> Maybe Integer
evalStr s = case parseExp Lit ExprT.Add ExprT.Mul s of
  Nothing -> Nothing
  Just x -> Just (eval x)

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = ExprT.Add
  mul = ExprT.Mul

m1 :: ExprT
m1 = mul (add (lit 2) (lit 3)) (lit 4)

m2 :: ExprT
m2 = ExprT.Mul (ExprT.Add (Lit 2) (Lit 3)) (Lit 4)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

newtype MinMax = MinMax Integer deriving (Eq, Show)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add = \(MinMax a) (MinMax b) -> MinMax (max a b)
  mul = \(MinMax a) (MinMax b) -> MinMax (min a b)

instance Expr Mod7 where
  lit = \x -> Mod7 (mod (mod x 7 + 7) 7)
  add = \(Mod7 a) (Mod7 b) -> lit (a + b)
  mul = \(Mod7 a) (Mod7 b) -> lit (a * b)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer

testBool :: Maybe Bool
testBool = testExp :: Maybe Bool

testMM :: Maybe MinMax
testMM = testExp :: Maybe MinMax

testSat :: Maybe Mod7
testSat = testExp :: Maybe Mod7

instance Expr Program where
  lit = \x -> [StackVM.PushI x]
  add = \a b -> a ++ b ++ [StackVM.Add]
  mul = \a b -> a ++ b ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul
