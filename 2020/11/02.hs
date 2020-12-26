import Data.Map
import Data.List.Split
import Data.Char

data Point = Occupied | Empty | Floor deriving (Eq)

processChar '.' = (Floor, 0)
processChar 'L' = (Empty, 0)

processLine [] y x m = m
processLine (c:l) y x m =
  processLine l y (x+1) (insert (x,y) (processChar c) m)

processLines [] y m = m
processLines (l:ls) y m =
  processLines ls (y+1) (processLine l y 0 m)

adjustVisible f (x,y) (dx,dy) m =
  let (nx,ny) = (x+dx, y+dy) in
  case Data.Map.lookup (nx,ny) m of
     Just (Floor,_) -> adjustVisible f (nx,ny) (dx,dy) m
     Nothing -> m
     otherwise -> adjust f (nx,ny) m

adjustVisibles f (x,y) m =
  let fv = adjustVisible f (x,y) in
  fv (-1,-1) $ fv (0,-1) $ fv (1,-1) $
  fv (-1,0) $ fv (1,0) $
  fv (-1,1) $ fv (0,1) $ fv (1,1) $
  m

mapPoint _ (Floor, _) m = m
mapPoint (x,y) (Empty,0) m =
  adjust (\ (_,n) -> (Occupied, n)) (x,y) $
  adjustVisibles (\ (p,n) -> (p,n+1)) (x,y) $ m
mapPoint (x,y) (Empty,n) m = m
mapPoint (x,y) (Occupied,n) m
  | n < 5 = m
  | otherwise = adjust (\ (_,n) -> (Empty, n)) (x,y) $
                adjustVisibles (\ (p,n) -> (p,n-1)) (x,y) $ m

iteration m = foldrWithKey mapPoint m m

repeatIteration m =
  let m2 = iteration m in
  if m == m2 then m2
  else repeatIteration m2

pointToChar Floor = '.'
pointToChar Empty = 'L'
pointToChar Occupied = '#'

lineToString (x,y) m =
  case Data.Map.lookup (x,y) m of
    Nothing -> ""
    Just (p,_) -> pointToChar p : lineToString (x+1,y) m

mapToString y m =
  case Data.Map.lookup (0,y) m of
    Nothing -> ""
    Just _ -> lineToString (0,y) m ++ "\n" ++ mapToString (y+1) m  

countPoint (Floor,_) n = n
countPoint (Empty,_) n = n
countPoint (Occupied,_) n = n+1  

main = do
  input <- getContents
  let m = processLines (splitOn "\n" input) 0 Data.Map.empty

  let finalSeats = repeatIteration m
  -- putStrLn (mapToString 0 finalSeats)
  print (Data.Map.foldr countPoint 0 finalSeats)
