import Data.List
import Data.List.Split
import Data.Map


splitOnAll :: [String] -> String -> [String]
splitOnAll [] l = [l]
splitOnAll (s:ss) l =
  let ls = splitOn s l in
  Prelude.foldr (++) [] (Prelude.map (splitOnAll ss) ls)

processLine :: String -> (Int, Int)
processLine s =
  let (n:p:_) = splitOnAll ([" players; last marble is worth "," points"]) s in
  (read n, read p)


data CircleList = CircleList ([Int]) ([Int]) deriving Show

goNext :: CircleList -> CircleList
goNext (CircleList xs [y]) = CircleList [] (reverse (y:xs))
goNext (CircleList xs (y:ys)) = CircleList (y:xs) ys

goNextN :: CircleList -> Int -> CircleList
goNextN cs 0 = cs
goNextN cs n = goNextN (goNext cs) (n-1)

goBack :: CircleList -> CircleList
goBack (CircleList [] ys) = let (x:xs) = reverse ys in CircleList xs [x]  
goBack (CircleList (x:xs) ys) = CircleList xs (x:ys)

goBackN :: CircleList -> Int -> CircleList
goBackN cs 0 = cs
goBackN cs n = goBackN (goBack cs) (n-1) 

get :: CircleList -> Int
get (CircleList xs (y:ys)) = y

insert :: CircleList -> Int -> CircleList
insert (CircleList xs ys) y = CircleList xs (y:ys)

del :: CircleList -> CircleList
del (CircleList xs [y]) = CircleList [] (reverse xs)
del (CircleList xs (y:ys)) = CircleList xs ys

checkCondition i = mod i 23 == 0
addMod i d = mod (i+1) (d)

xOrAdd v Nothing = Just (v)
xOrAdd v (Just x) = Just (x+v) 
mapAdd k v m = Data.Map.alter (xOrAdd v) k m

playGame :: CircleList -> Map Int Int -> Int -> Int -> Int -> Int -> Int
playGame cs ps p i c n | i > n = Data.Map.foldr (\a b -> max a b) minBound ps
playGame cs ps p i c n | checkCondition i =
  let cs' = goBackN cs 7 in
  let x = get cs' in
  let cs'' = del cs' in
  playGame cs'' (mapAdd p (x+i) ps) (addMod p c) (i+1) c n
  
playGame cs ps p i c n =
  let cs' = goNextN cs 2 in
  let cs'' = Main.insert cs' i in
  playGame cs'' ps (addMod p c) (i+1) c n

main = do
  input <- getContents
  let (p,n) = processLine input
  print $ playGame (CircleList [] [0]) Data.Map.empty 0 1 p n
  

