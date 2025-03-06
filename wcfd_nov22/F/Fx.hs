import Data.Map as Map

build_table_aux :: Int -> Int -> Int -> Map Int [(Int,Int)] -> Map Int [(Int,Int)]
build_table_aux n i j m =
  if j > n `div` i then m
  else Map.insert (i*j) ([(i,j)]) $ build_table_aux n i (j+1) m 

build_table :: Int -> Int -> Map Int [(Int,Int)] -> Map Int [(Int,Int)]
build_table n i m =
  if i > n then m
  else build_table n (i+1) $ build_table_aux n i i m

eval_member :: Int -> Int -> Map Int [(Int,Int)] -> Map Int [(Int,Int)]
eval_member i r m =
  if r > i `div` 2 then m
  else if member r m && member (i-r) m
       then insert i (findWithDefault [] r m ++ findWithDefault [] (i-r) m) m
       else eval_member i (r+1) m

iter_table :: Int -> Int -> Map Int [(Int,Int)] -> Map Int [(Int,Int)]
iter_table n i m = 
  if i > n then m
  else
  if notMember i m
  then iter_table n (i+1) $ eval_member i 1 m
  else iter_table n (i+1) m
  
parse_output_aux :: [(Int,Int)] -> String
parse_output_aux ((x,y):[]) = (show x)++"x"++(show y)
parse_output_aux ((x,y):z:xs) = (show x)++"x"++(show y)++" "++parse_output_aux (z:xs)
parse_output :: Maybe [(Int,Int)] -> String
parse_output Nothing = "impossible"
parse_output (Just xs) = parse_output_aux xs

main = do
  input <- getLine
  let n = read input
  let m = build_table n 2 Map.empty
  let m' = iter_table n 0 m
  print (parse_output (Map.lookup n m'))
