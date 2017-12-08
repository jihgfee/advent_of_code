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

eval [] m = m
eval ((r,d,(cr,op,cv)):ls) m = 
  let (rv,m') = if member r m then (findWithDefault 0 r m, m) else (0, insert r 0 m) in
  let m'' = if (evalOp ((findWithDefault 0 cr m'),op, cv)) then adjust (evalDir d) r m' else m' in
  eval ls m''

main = do
  input <- getContents
  let l = Prelude.map (\l -> convertLine (words l)) (lines input)
  print (maximum (elems (eval l empty)))
