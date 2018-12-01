import Data.List.Split;

mkScanners :: [String] -> Integer -> [Maybe (Integer,Integer)]
mkScanners [] i = []
mkScanners (s:ss) i = 
  let s' = splitOn ": " s in
  let (id,len) = (read (head s'), read (head (tail s'))) in
  if i == id 
    then Just (id,len) : mkScanners ss (i+1)
    else Nothing : mkScanners (s:ss) (i+1)

checkHit Nothing i = 0
checkHit (Just (d,l)) i = if (mod i ((l*2)-2)) == 0 then 1 else 0

walk [] i = 0
walk (s:ss) i = checkHit s i + walk ss (i+1)


lowestNeededDelay ss d =
  let isSuccess = walk ss d == 0 in
  if isSuccess then d else lowestNeededDelay ss (d+1)



main = do
  input <- getContents
  --print (length (mkScanners (lines input) 0))
  print (lowestNeededDelay (mkScanners (lines input) 0) 0)
