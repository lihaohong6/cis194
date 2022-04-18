module TestHw7 where
import Hw7
import Sized (Size)

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:_) !!? 0 = Just x
(_:xs) !!? i = xs !!? (i-1)

propIndexJ :: Int -> JoinList Size Int -> Bool
propIndexJ i jl = indexJ i jl == (jlToList jl !!? i)

propDropJ :: Int -> JoinList Size Int -> Bool
propDropJ n jl = jlToList (dropJ n jl) == drop n (jlToList jl)

propTakeJ :: Int -> JoinList Size Int -> Bool
propTakeJ n jl = jlToList (takeJ n jl) == take n (jlToList jl)


