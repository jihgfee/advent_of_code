import Data.Map

incr i 0 m  = m
incr i n m =
  if member i m
    then incr (i+1) (n-1) (adjust (+1) i m)
    else incr 0 n m

max m = foldlWithKey (\(k',v') k v -> if v > v' then (k,v) else (k',v')) (size m,-1) m 

redist m ms i =
  if member m ms
    then findWithDefault (-1) m ms
    else let (k,v) = Main.max m in
         redist (incr (k+1) v (adjust (\v->0) k m)) (insert m 1 (Data.Map.map (+1) ms)) (i+1)

listToMap l = (fst (Prelude.foldl (\(m,i) l -> (insert i (read l) m,i+1)) (empty, 0) l))

main = do
  input <- getContents
  print (redist (listToMap (words input)) empty 0)
