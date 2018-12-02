import Data.Char
import Data.List
import Data.Set
import Data.Map

mapAdd :: Int -> Map Int Int -> Map Int Int
mapAdd a m =
  Data.Map.insert a ((Data.Map.findWithDefault 0 a m)+1) m  

mapAddAll :: [Int] -> Map Int Int -> Map Int Int
mapAddAll [] m = m
mapAddAll (a:s) m = mapAddAll s (mapAdd a m)

getSums' :: [Char] -> Char -> Int -> Set Int -> Set Int
getSums' [] c' a s = (Data.Set.insert a s)
getSums' (c:cs) c' a s = if c == c' then getSums' cs c (a+1) s
                         else getSums' cs c 1 (Data.Set.insert a s)

getSums :: [Char] -> Set Int
getSums (c:cs) = getSums' cs c 1 Data.Set.empty

getAllSums' :: [String] -> Map Int Int -> Map Int Int
getAllSums' [] m = m
getAllSums' (s:ss) m =
  getAllSums' ss (mapAddAll (Data.Set.toList (getSums s)) m)

getAllSums :: [String] -> Map Int Int
getAllSums ss =
  getAllSums' ss (Data.Map.empty)

main = do
  input <- getContents
  let ls = (Data.List.map sort (lines input))
  let m = getAllSums ls
  print ((findWithDefault 0 2 m) * (findWithDefault 0 3 m))
