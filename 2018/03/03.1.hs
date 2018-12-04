import Data.List.Split;
import Data.Map;

data Entry = Entry String Int Int Int Int Int Int deriving (Show)

mapIncr :: (Int,Int) -> Map (Int,Int) Int -> Map (Int,Int) Int
mapIncr a m =
  Data.Map.insert a ((Data.Map.findWithDefault 0 a m)+1) m  


processLine l =
  let ((_:id):l':[]) = splitOn " @ " l in
  let (xy:wh:[]) = splitOn ": " l' in
  let (x:y:[]) = splitOn "," xy in
  let (w:h:[]) = splitOn "x" wh in
  Entry id (read x) (read y) (read w) (read h) (read w) (read h)

buildMap :: [Entry] -> Map (Int,Int) Int -> Map (Int,Int) Int
buildMap [] m = m
buildMap ((Entry i x y w h 1 1):ls) m = buildMap ls (mapIncr (x,y) m)
buildMap ((Entry i x y w h 1 ch):ls) m =
  buildMap ((Entry i x y w h w (ch-1)):ls) (mapIncr (x,y+ch-1) m)
buildMap ((Entry i x y w h cw ch):ls) m =
  buildMap ((Entry i x y w h (cw-1) ch):ls) (mapIncr (x+cw-1,y+ch-1) m)

extractMap :: [(Int,Int)] -> Map (Int,Int) Int -> Int
extractMap [] m = 0
extractMap (k:ks) m =
  (if ((Data.Map.findWithDefault 0 k m) > 1) then 1 else 0) + (extractMap ks m)

main = do
  input <- getContents
  let ls = lines input
  let ls' = Prelude.map processLine ls
  let m = buildMap ls' (Data.Map.empty)
  -- print ls'
  -- print (m)
  print (extractMap (keys m) m)
