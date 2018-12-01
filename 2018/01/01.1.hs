import Data.Char

stripChar ('+':cs) = cs
stripChar cs = cs

main = do 
  input <- getContents
  print (foldl (\a c -> a+c) 0 (map (\l -> read (stripChar l)) (lines input)))
