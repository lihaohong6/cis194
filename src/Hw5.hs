module Hw5 where
import Parser

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
  deriving (Show, Eq)

eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y

evalStr :: String -> Maybe Integer
evalStr s = let parsed = parseExp Lit Add Mul s
  in case parsed of
    Nothing -> Nothing
    Just x -> Just (eval x)


