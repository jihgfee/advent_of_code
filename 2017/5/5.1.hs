import Data.Map

steps' :: Map Integer Integer -> Integer -> Integer -> Integer
steps' m i n = 
  if (member i m)
    then steps' (adjust (\i -> i+1) i m) (i + (findWithDefault 0 i m)) (n+1)
    else n

steps :: Map Integer Integer -> Integer
steps m = steps' m 0 0

main = do
  input <- getContents
  print (steps (fst (Prelude.foldl (\(m,i) l -> (insert i (read l) m,i+1)) (empty, 0) (lines (input)))))
