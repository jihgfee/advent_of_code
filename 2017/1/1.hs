import Data.Char

main = do 
  input <- getContents
  let l1 = map digitToInt (filter isDigit input)
  let offset = div (length l1) 2
  let l2 = (drop offset l1) ++ (take offset l1)
  let l = zip l1 l2
  print (foldl (\a (x,y) -> if x==y then a+x else a) 0 l)
