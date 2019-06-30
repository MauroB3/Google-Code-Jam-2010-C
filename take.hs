import Data.List

howMany xs n = foldr (\x r n -> if n < x then 0 else 1 + r (n-x)) (\_ -> 0) xs n

reorder xs n = drop n xs ++ take n xs
