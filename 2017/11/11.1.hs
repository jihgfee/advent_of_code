import Data.List.Split;

findChildPos p [] = p
findChildPos (x,y) ("n":ds) = findChildPos (x,y+1) ds
findChildPos (x,y) ("nw":ds) = findChildPos (x-0.5,y+0.5) ds
findChildPos (x,y) ("ne":ds) = findChildPos (x+0.5,y+0.5) ds
findChildPos (x,y) ("sw":ds) = findChildPos (x-0.5,y-0.5) ds
findChildPos (x,y) ("se":ds) = findChildPos (x+0.5,y-0.5) ds
findChildPos (x,y) ("s":ds) = findChildPos (x,y-1) ds
findChildPos p (d:ds) = findChildPos p ds

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
  print (shortestPath (findChildPos (0,0) (splitOn "," input)))
  
