
main = do
  input <- getContents
  print (foldl (+) 0 (map (\l -> (maximum l) - (minimum l)) (map (\l -> map read (words l)) (lines input))))
