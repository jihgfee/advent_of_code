import Data.Map

main = do
  input <- getContents
  print (fromList (Prelude.map read (lines (input))))
