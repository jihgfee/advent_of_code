import Data.Char

main = do 
  input <- getContents
  let l =  map digitToInt (filter isDigit input)
  print (fst (foldl (\(a,b) x -> (if b==x then a+x else a,x)) (0,last l) l))
