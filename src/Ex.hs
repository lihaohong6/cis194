module Ex where

import Data.Char (chr)

carpet :: Int -> String
carpet n =
  let row = reverse [0 .. n] ++ [1 .. n]
   in unlines
        ( map
            (\x -> map (\y -> chr (48 + max x y)) row)
            row
        )

data Nat = Zero | S Nat
  deriving Eq

toInt :: Nat -> Integer
toInt Zero = 0
toInt (S a) = toInt a + 1

instance Show Nat where
  show n = show (toInt n)

instance Ord Nat where
  Zero <= _ = True
  (S _) <= Zero = False
  (S a) <= (S b) = a <= b

instance Num Nat where
  Zero + a = a
  (S a) + b = S (a + b)
  a - Zero = a
  Zero - _ = Zero
  (S a) - (S b) = a - b
  Zero * _ = Zero
  (S a) * b = b + (a * b)
  signum Zero = 0
  signum _ = 1
  fromInteger 0 = Zero
  fromInteger a
    | a < 0 = Zero
    | otherwise = fromInteger (a - 1)
  abs = id
