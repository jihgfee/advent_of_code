import Data.Char

main = do 
  input <- getContents
  let l1 = map digitToInt (filter isDigit input)
  let l2 = (drop (div (length l1) 2) l1) ++ (take (div (length l1) 2) l1)
  let l = zip l1 l2
  print (foldl (\a (x,y) -> if x==y then a+x else a) 0 l)
