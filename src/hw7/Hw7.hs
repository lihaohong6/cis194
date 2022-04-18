module Hw7 where

import Sized

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append (tag x <> tag y) x y

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ 0 (Single _ x) = Just x
indexJ _ (Single _ _) = Nothing
indexJ index (Append _ a b)
  | index < 0 = Nothing
  | otherwise =
    let sizeA = getSize (size (tag a))
     in if index > sizeA
          then indexJ (index - sizeA) b
          else indexJ index a

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ a b) = jlToList a ++ jlToList b

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ 0 x = x
dropJ _ (Single _ _) = Empty
dropJ num (Append m a b) =
  let sz = getSize (size m)
   in if num >= sz
        then Empty
        else
          let sizeA = getSize (size (tag a))
           in if num > sizeA
                then dropJ (num - sizeA) b
                else dropJ num a +++ b

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 _ = Empty
takeJ _ Empty = Empty
takeJ _ s@(Single _ _) = s
takeJ num original@(Append m a b) =
  let sz = getSize (size m)
  in if num >= sz
    then original
    else let sizeLeft = getSize $ size (tag a)
      in if num >= sizeLeft
        then a +++ takeJ (num - sizeLeft) b
        else takeJ num a
