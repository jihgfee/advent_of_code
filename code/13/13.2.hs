import Data.List.Split;


mkScanners [] i = []
mkScanners (s:ss) i = 
  let s' = splitOn ": " s in
  let (id,len) = (read (head s'), read (head (tail s'))) in
  if i == id 
    then Just (0,len,id,True) : mkScanners ss (i+1)
    else Nothing : mkScanners (s:ss) (i+1)

updateScanner Nothing = Nothing
--updateScanner (Just (p,l,d,o)) = Just (p+o,l,d, if (p+o) == (l-1) then -o else o)
updateScanner (Just (p,l,d,True)) = Just (p+1,l,d, if (p+1) == (l-1) then False else True)
updateScanner (Just (p,l,d,False)) = Just (p-1,l,d, if (p-1) == 0 then True else False)

updateScanners [] = []
updateScanners (s:ss) = updateScanner s : updateScanners ss

checkHit Nothing = 0
checkHit (Just (p,l,d,o)) = if p == 0 then l*d else 0

walk [] = 0
walk (s:ss) = checkHit s + walk (updateScanners ss) 

delay ss 0 = ss
delay ss n = delay (updateScanners ss) (n-1)

lowestNeededDelay ss d =
  let ss' = delay ss d in
  let isSuccess = walk ss' == 0 in
  if isSuccess then d else lowestNeededDelay ss (d+1)

lowestNeededDelay2 ss = 

main = do
  input <- getContents
  --print (length (mkScanners (lines input) 0))
  print (lowestNeededDelay (mkScanners (lines input) 0) 0)
