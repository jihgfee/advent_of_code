import Data.Map

convertLine (r:d:v:_:cr:op:cv:[]) =
  (r,(d,read v),(cr,op,read cv))

evalOp (cr,">",cv) = cr > cv
evalOp (cr,"<",cv) = cr < cv
evalOp (cr,">=",cv) = cr >= cv
evalOp (cr,"<=",cv) = cr <= cv
evalOp (cr,"==",cv) = cr == cv
evalOp (cr,"!=",cv) = cr /= cv
evalOp (cr,op,cv) = False
  
evalDir ("inc",v) = (\a -> a+v)
evalDir ("dec",v) = (\a -> a-v)
evalDir (d,v) = (\a -> a)

eval [] m n = n
eval ((r,d,(cr,op,cv)):ls) m n = 
  let (rv,m') = if member r m then (findWithDefault 0 r m, m) else (0, insert r 0 m) in
  if (evalOp ((findWithDefault 0 cr m'),op, cv))
    then
      let m'' = adjust (evalDir d) r m' in
      let v = findWithDefault 0 r m'' in
      if v > n then eval ls m'' v else eval ls m'' n
    else
      eval ls m' n

main = do
  input <- getContents
  let l = Prelude.map (\l -> convertLine (words l)) (lines input)
  print (eval l empty 0)
