import Data.Char
import Data.Set

checkDual c c' = toUpper c == c' && c == toLower c' || toLower c == c' && c == toUpper c'

strip [] = []
strip (c:[]) = (c:[])
strip (c:c':s) =
  if (checkDual c c') then strip s else (c:strip (c':s))

repeatStrip :: String -> String
repeatStrip s =
  let s' = strip s in
  if (length s' == length s) then s' else repeatStrip s'

getUnits :: String -> Set Char -> Set Char
getUnits [] s = s
getUnits (c:cs) s =
  getUnits cs (insert (toLower c) s)

removeUnits [] c = []
removeUnits (c':cs) c = if (toLower c' == c) then (removeUnits cs c) else (c' : removeUnits cs c)

main = do
  input <- getContents
  --let res = repeatStrip input
  --print (res)
  --print (length res)
  let units = Data.Set.toList (getUnits input Data.Set.empty)
  --print units
  let no_unit_strings = Prelude.map (\c -> removeUnits input c) units
  --print no_unit_strings
  let strings = Prelude.map repeatStrip no_unit_strings
  --print strings
  let res = Prelude.foldr min maxBound (Prelude.map length strings)
  print res
