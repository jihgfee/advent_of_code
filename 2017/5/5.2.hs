import Data.Map

steps' :: Map Integer Integer -> Integer -> Integer -> Integer
steps' m k n = 
  if (member k m)
    then steps' (adjust (\v -> v + (if findWithDefault 0 k m >= 3 then -1 else 1)) k m) (k + findWithDefault 0 k m) (n+1)
    else n

steps :: Map Integer Integer -> Integer
steps m = steps' m 0 0

main = do
  input <- getContents
  print (steps (fst (Prelude.foldl (\(m,i) l -> (insert i (read l) m,i+1)) (empty, 0) (lines (input)))))
