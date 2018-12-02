import Data.Char
import Data.List
import Data.Set
import Data.Map

unionString :: [Char] -> [Char] -> [Char]
unionString [] [] = ""
unionString (c:cs) (c':cs') =
  if (c == c') then (c : unionString cs cs')
  else unionString cs cs'

diffString :: [Char] -> [Char] -> Int -> Bool
diffString [] [] i = i <= 1
diffString (c:cs) (c':cs') i = 
   if (c == c') then diffString cs cs' i
   else diffString cs cs' (i+1)

iterAll' :: String -> [String] -> Maybe String
iterAll' s [] = Nothing
iterAll' s (s':ss) =
  if (diffString s s' 0) then Just (unionString s s')
  else iterAll' s ss

iterAll :: [String] -> Maybe String
iterAll [] = Nothing
iterAll (s:ss) =
  let res = iterAll' s ss in
  if (res == Nothing) then iterAll ss
  else res

main = do
  input <- getContents
  print (iterAll (lines input))
