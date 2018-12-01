import Data.Map;
import Data.List.Split;
import Data.Char (ord);
import Data.Bits;
import Numeric (showHex);

postfix = [17, 31, 73, 47, 23]

duplicate 0 l = []
duplicate n l = l ++ (duplicate (n-1) l)

listToMap [] m = m
listToMap (l:ls) m = listToMap ls (insert l l m)

mapToList n m = 
  if n == size m
    then []
    else (findWithDefault (-1) n m) : (mapToList (n+1) (m)) 

incrWrap n i c = mod (n+i) c

--listFromMapPos :: Int -> Int -> Map Int Int -> [Int]
listFromMapPos p 0 m = []
listFromMapPos p l m = (findWithDefault (-1) p m) : (listFromMapPos (incrWrap p 1 (size m)) (l-1) m)

insertListFromPos p [] m = m
insertListFromPos p (l:ls) m = insertListFromPos (incrWrap p 1 (size m)) ls (adjust (\a -> l) p m)

process p s m [] = m
process p s m (l:ls) = 
  process (incrWrap p (l+s) (size m)) (s+1) (insertListFromPos p (reverse (listFromMapPos p l m)) m) ls   

toDense' (l:[]) = l
toDense' (l:ls) = xor l (toDense' ls)
toDense [] = []
toDense ls = (toDense' (Prelude.take 16 ls)) : toDense (Prelude.drop 16 ls)

fixHex (c:[]) = '0':c:[]
fixHex s = s

toHex x = fixHex (showHex x "")

main = do
  input <- getContents
  {-
  let l = duplicate 64 ((Prelude.map ord (init input)) ++ postfix)
  let m = process 0 0 (listToMap [0..255] empty) l
  let l' = mapToList 0 m
  let s = toSparse l'
  let a = (Prelude.map (\i -> showHex i "") s)
  -}
  print (Prelude.map toHex (toDense (mapToList 0 (process 0 0 (listToMap [0..255] empty) (duplicate 64 ((Prelude.map ord (init input)) ++ postfix))))))
