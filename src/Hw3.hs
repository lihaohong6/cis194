module Hw3 where

import Data.Char (chr)

buildList :: [t] -> Int -> [t]
buildList list num = case drop (num -1) list of
  [] -> []
  x : xs -> x : buildList xs num

skips :: [t] -> [[t]]
skips list = map (buildList list) [1 .. length list]

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_] = []
localMaxima [_, _] = []
localMaxima (a : b : c : xs) =
  if a < b && b > c
    then b : localMaxima (b : c : xs)
    else localMaxima (b : c : xs)

localMaxima2 :: [Integer] -> [Integer]
localMaxima2 list =
  map
    (\(_, b, _) -> b)
    ( filter
        (\(a, b, c) -> a < b && b > c)
        (zip3 list (drop 1 list) (drop 2 list))
    )

histogram :: [Integer] -> String
histogram list =
  let occurrences = map (\num -> length (filter (== num) list)) [0 .. 9]
   in unlines
        ( reverse
            ( map chr [48 .. 57] :
              replicate 10 '=' :
              map (\cur -> map (\o -> if o >= cur then '*' else ' ') occurrences) [1 .. maximum occurrences]
            )
        )
