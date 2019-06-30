import System.IO
import Data.List

main :: IO ()
main =
 do
  sourceFile <- readFile "C-small-practice.in"
  --sourceFile <- readFile "C-small-practice.in"
  let  numberOfCases = head(lines sourceFile)
       cases = takeCases (tail (lines sourceFile))
       result1 = map startRides' cases 
       result = foldr (\x r-> "Case #"  ++ show(fst x) ++ ": " ++ show(snd x) ++ "\n" ++ r) [] (enumCases 1 result1)
   in
      writeFile "./resultLarge.txt" result


takeCases :: [String] -> [([Int], [Int])]
takeCases [] = []
takeCases (x:y:xs) = (map (\s -> read s :: Int) (words x), map (\s -> read s :: Int) (words y) ) : takeCases xs  


enumCases :: Int-> [Int] -> [(Int,Int)]
enumCases n [] = []
enumCases n (x:xs) = (n,x) : enumCases (n+1) xs


takeQueue :: Int -> [Int] -> [Int]
takeQueue n [] = []
takeQueue n (x:xs) = let cont = (n - x) in 
  if(cont >= 0)
    then x : takeQueue cont xs
    else []

orderQueue :: [Int] -> [Int] -> [Int]
orderQueue xs ys = drop (length xs) ys ++ xs

howMany xs n = foldr (\x r n -> if n < x then 0 else 1 + r (n-x)) (\_ -> 0) xs n

reorder xs n = drop n xs ++ take n xs

-- makeRide :: Int -> [Int] -> Int
-- makeRide n [] = 0
-- makeRide n xs = foldl' (+) 0 (take n )

startRides' :: ([Int],[Int]) -> Int
startRides' ([0, k, g], xs) = 0
startRides' ([r, k, g], xs) = foldl' (+) 0 t + startRides' ([r - 1, k, g], d ++ t)
   where n = howMany xs k
         t = take n xs
         d = drop n xs

--startRides :: Int -> ([Int], [Int]) -> Int
--startRides n ([0, k, g], xs) = n
--startRides n ([r, k, g], xs) = startRides (n + makeRide k xs) ([r - 1, k, g], takeQueue k (cycle xs))
