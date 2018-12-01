
allUnique xs = snd (foldl (\(l,b) x -> if elem x l then (x:l,False) else (x:l, b)) ([], True) xs)
countElem n xs = foldl (\i x -> if x == n then (i+1) else i) 0 xs 

main = do
  input <- getContents
  print (countElem True (map (\l -> allUnique (words l)) (lines input)))
  
