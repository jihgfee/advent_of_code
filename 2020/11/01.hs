import Data.Map
import Data.List.Split
import Data.Char

data Point = Occupied Int | Empty Int | Floor deriving (Eq)

instance Show Point where
  show (Occupied n) = "#"
  show (Empty n) = "L"
  show Floor = "."

processChar '.' = Floor
processChar 'L' = Empty (0)

processLine [] y x m = m
processLine (c:l) y x m =
  processLine l y (x+1) (insert (x,y) (processChar c) m)

processLines [] y m = m
processLines (l:ls) y m =
  processLines ls (y+1) (processLine l y 0 m)

incrPoint Floor = Floor
incrPoint (Occupied n) = Occupied (n+1)
incrPoint (Empty n) = Empty (n+1)

decrPoint Floor = Floor
decrPoint (Occupied n) = Occupied (n-1)
decrPoint (Empty n) = Empty (n-1)

adjustAdjacents f (x,y) m =
  adjust f (x-1,y-1) $
  adjust f (x,y-1) $
  adjust f (x+1,y-1) $
  adjust f (x-1,y) $
  adjust f (x+1,y) $
  adjust f (x-1,y+1) $
  adjust f (x,y+1) $
  adjust f (x+1,y+1) $
  m

setOccupied (Empty n) = Occupied n
setEmpty (Occupied n) = Empty n

mapPoint _ Floor m = m
mapPoint (x,y) (Empty 0) m = adjust setOccupied (x,y) $
                             adjustAdjacents incrPoint (x,y) $
                             m
mapPoint (x,y) (Empty n) m = m
mapPoint (x,y) (Occupied n) m
  | n < 4 = m
  | otherwise = adjust setEmpty (x,y) $
                adjustAdjacents decrPoint (x,y) $ m

iteration m = foldrWithKey mapPoint m m

repeatIteration m =
  let m2 = iteration m in
  if m == m2 then m2
  else repeatIteration m2

pointToChar Floor = '.'
pointToChar (Empty n) = 'L'
pointToChar (Occupied n) = '#'

lineToString (x,y) m =
  case Data.Map.lookup (x,y) m of
    Nothing -> ""
    Just p -> pointToChar p : lineToString (x+1,y) m

mapToString y m =
  case Data.Map.lookup (0,y) m of
    Nothing -> ""
    Just p -> lineToString (0,y) m ++ "\n" ++ mapToString (y+1) m  

countPoint Floor n = n
countPoint (Empty _) n = n
countPoint (Occupied _) n = n+1  

main = do
  input <- getContents
  let m = processLines (splitOn "\n" input) 0 Data.Map.empty

  let finalSeats = repeatIteration m
  -- putStrLn (mapToString 0 finalSeats)
  print (Data.Map.foldr countPoint 0 finalSeats) 
