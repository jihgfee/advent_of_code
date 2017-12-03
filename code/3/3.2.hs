import Data.Map

rotate (1,0) = (0,1)
rotate (0,1) = (-1,0)
rotate (-1,0) = (0,-1)
rotate (0,-1) = (1,0)

add (i,j) (x,y) = (i+x,j+y)
mul (x,y) i = (x*i,y*i)

sumNeighbour (x,y) m =
  findWithDefault 0 (x+1,y)   m +
  findWithDefault 0 (x+1,y+1) m +
  findWithDefault 0 (x,y+1)   m +
  findWithDefault 0 (x-1,y+1) m +
  findWithDefault 0 (x-1,y)   m +
  findWithDefault 0 (x-1,y-1) m +
  findWithDefault 0 (x,y-1)   m +
  findWithDefault 0 (x+1,y-1) m

loc' n m pos turn steps length rot =
  let i = sumNeighbour pos m in
  if i > n 
    then i
    else 
      if steps == length-1 then
        if turn == 1 then
          loc' n (insert pos i m) (add pos rot) 0 0 (length+1) (rotate rot) 
        else 
          loc' n  (insert pos i m) (add pos rot) (turn+1) 0 length (rotate rot) 
      else
        loc' n  (insert pos i m) (add pos rot) turn (steps+1) length rot   

loc n = loc' n (insert (0,0) 1 empty) (1,0) 1 0 1 (0,1)

main = do
  input <- getContents
  let l = loc (read input)
  print (l)

