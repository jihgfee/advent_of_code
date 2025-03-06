import Data.Map as Map

build_table_aux :: Int -> Int -> Int -> Map Int [(Int,Int)] -> Map Int [(Int,Int)]
build_table_aux n i j m =
  if j <= n `div` i then
    let m' = Map.insert (i*j) ([(i,j)]) m in
    build_table_aux n i (j+1) m'
  else m

build_table :: Int -> Int -> Map Int [(Int,Int)] -> Map Int [(Int,Int)]
build_table n i m =
  if i <= n `div` 2 then
    let m' = build_table_aux n i i m in
    build_table n (i+1) m'
  else m

eval_member :: Int -> Int -> Map Int [(Int,Int)] -> Map Int [(Int,Int)]
eval_member i r m =
  if r > i `div` 2 then m
  else if member r m && member (i-r) m then
        insert i (findWithDefault [] r m ++ findWithDefault [] (i-r) m) m
       else eval_member i (r+1) m

iter_table :: Int -> Int -> Map Int [(Int,Int)] -> Map Int [(Int,Int)]
iter_table n i m = 
  if i <= n then
    if notMember i m then
      let m' = eval_member i 1 m in
      iter_table n (i+1) m'
    else iter_table n (i+1) m
  else m

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
