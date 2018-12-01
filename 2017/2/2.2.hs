import Data.List

divs x y = mod x y == 0

f'' x [] = Nothing
f'' x (y:ys) = if divs x y then Just (div x y) else f'' x ys  

f' [] = Nothing
f' (x:xs) = 
  let res = f'' x xs in
  if res == Nothing then f' xs else res

optionToInt Nothing = 0
optionToInt (Just x) = x

f l = optionToInt (f' (reverse (sort l)))

main = do
  input <- getContents
  print (foldl (+) 0 (map (\l -> f (map read (words l))) (lines input)))
