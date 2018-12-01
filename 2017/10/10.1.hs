import Data.Map;
import Data.List.Split;
{-
sequence (l:ls) 0 = l:[]
sequence (l:ls) len = l:(sequence l (len-1))
rev 0 ls len = 
  let l' = reverse (sequence ls len) in
  rev 0 ls len

rev p (l:ls) len = l:(rev (p-1) ls len)

process p s l' [] = l
process p s l (len:ls) = 
  let l' = 
-}

listToMap [] m = m
listToMap (l:ls) m = listToMap ls (insert l l m)

incrWrap n i c = mod (n+i) c

listFromMapPos :: Int -> Int -> Map Int Int -> [Int]
listFromMapPos p 0 m = []
listFromMapPos p l m = (findWithDefault (-1) p m) : (listFromMapPos (incrWrap p 1 (size m)) (l-1) m)

insertListFromPos p [] m = m
insertListFromPos p (l:ls) m = insertListFromPos (incrWrap p 1 (size m)) ls (adjust (\a -> l) p m)

process p s m [] = m
process p s m (l:ls) = 
  process (incrWrap p (l+s) (size m)) (s+1) (insertListFromPos p (reverse (listFromMapPos p l m)) m) ls   

res m = (findWithDefault (-1) 0 m) * (findWithDefault (-1) 1 m)



main = do
 input <- getContents
 print (res (process 0 0 (listToMap [0..255] empty) (Prelude.map read (splitOn "," input))))
