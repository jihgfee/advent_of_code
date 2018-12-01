import Data.Char
import Data.Set

stripChar ('+':cs) = cs
stripChar cs = cs

stream' :: [Int] -> [Int] -> [Int] 
stream' [] ys = stream' ys ys
stream' (x:xs) ys = x:(stream' xs ys)

stream :: [Int] -> [Int] 
stream xs = stream' [] xs

findFrequency' :: [Int] -> Int -> Set Int -> Int
findFrequency' (x:xs) a s =
  if (member a s) then a else
  findFrequency' xs (a+x) (insert a s)

findFrequency :: [Int] -> Int
findFrequency xs =
  findFrequency' (stream xs) 0 empty

main = do 
  input <- getContents
  print (findFrequency (Prelude.map (\l -> read (stripChar l)) (lines input)))
