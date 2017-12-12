import Data.List.Split;

coord (x,y) "n" = (x,y+1)
coord (x,y) "nw" = (x-0.5,y+0.5)
coord (x,y) "ne" = (x+0.5,y+0.5)
coord (x,y) "sw" = (x-0.5,y-0.5)
coord (x,y) "se" = (x+0.5,y-0.5)
coord (x,y) "s" = (x,y-1)
coord p d = p

findChildPos p [] = [p]
findChildPos p (d:ds) = p : findChildPos (coord p d) ds

shortestPath' :: (Float,Float) -> Int
shortestPath' (0.0,0.0) = 0
shortestPath' (x,y) =
  if x == 0 then (round (abs y))
  else 1 + (shortestPath' ((x-0.5), if (y > 0) then (y-0.5) else (y+0.5)))

shortestPath (x,y) = shortestPath' (abs x, abs y)

--let (ax,ay) = (abs x, abs y) in
  --(floor (div ax ay) * 2) + (floor (mod ax ay))  

main = do
  input <- getContents
  print (maximum (map shortestPath (findChildPos (0,0) (splitOn "," input))))
  
