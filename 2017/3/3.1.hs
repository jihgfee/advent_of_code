
rotate (1,0) = (0,1)
rotate (0,1) = (-1,0)
rotate (-1,0) = (0,-1)
rotate (0,-1) = (1,0)

add (i,j) (x,y) = (i+x,j+y)
mul (x,y) i = (x*i,y*i)

loc' n i pos turn steps length rot = 
  if n == i then
    pos
  else if steps == length-1 then
    if turn == 1 then
      loc' n (i+1) (add pos rot) 0 0 (length+1) (rotate rot) 
    else 
      loc' n (i+1) (add pos rot) (turn+1) 0 length (rotate rot) 
  else
    loc' n (i+1) (add pos rot) turn (steps+1) length rot   

loc n = loc' n 1 (0,0) 0 0 1 (1,0)
dist (x,y) = abs x + abs y

main = do
  input <- getContents
  let l = loc (read input)
  let d = dist l
  print (l,d)

