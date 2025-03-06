import Data.List.Split


splitOnAll :: [String] -> String -> [String]
splitOnAll [] l = [l]
splitOnAll (s:ss) l =
  let ls = splitOn s l in
  Prelude.foldr (++) [] (Prelude.map (splitOnAll ss) ls)


main = do
  input <- getContents
  let allLines = lines input;
  let splitLines = map tail $ splitOnAll [':',';'] allLines;
  print splitLines
  return ()

