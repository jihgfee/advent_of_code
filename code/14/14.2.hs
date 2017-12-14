import KnotHash;
import Data.Char;
import Numeric;
import Data.Map; 

toBit '0' = "0000"
toBit '1' = "0001"
toBit '2' = "0010"
toBit '3' = "0011"
toBit '4' = "0100"
toBit '5' = "0101"
toBit '6' = "0110"
toBit '7' = "0111"
toBit '8' = "1000"
toBit '9' = "1001"
toBit 'a' = "1010"
toBit 'b' = "1011"
toBit 'c' = "1100"
toBit 'd' = "1101"
toBit 'e' = "1110"
toBit 'f' = "1111"

toBits [] = []
toBits (c:cs) = toBit c ++ toBits cs

charToDigit '0' = 0
charToDigit '1' = 1

rows' l 128 = []
rows' l i = KnotHash.fromInput (l++"-"++(show i)) : rows' l (i+1) 
rows l = rows' (init l) 0

toMap' [] p m = m
toMap' (c:cs) (x,y) m = toMap' cs (x+1, y) (insert (x,y) (charToDigit c) m)
toMap [] p m = m 
toMap (l:ls) (x,y) m = toMap ls (x, y+1) (toMap' l (x,y) m) 

burn (x,y) m =
  let v = findWithDefault (-1) (x,y) m in
  if v == 1
    then burn (x,y-1) (burn (x-1,y) (burn (x,y+1) (burn (x+1,y) (adjust (\a -> 0) (x,y) m))))
    else m

--countRegion :: (Int,Int) -> (Int,Int) -> Map (Int,Int) Int -> Int
countRegion (x,y) (mx,my) m =
  if y >= my then 0 else if x >= mx then countRegion (0,y+1) (mx,my) m
  else let v = findWithDefault (-1) (x,y) m in 
       if v == 1
         then 1 + (countRegion (x+1,y) (mx,my) (burn (x,y) m))
         else countRegion (x+1,y) (mx,my) m

main = do
  input <- getContents
  let m = toMap (Prelude.map toBits (rows input)) (0,0) empty
  print (countRegion (0,0) (128,128) m)
