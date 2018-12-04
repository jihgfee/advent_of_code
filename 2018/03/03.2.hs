import Data.List.Split;
import Data.Map;
import Data.Set;

data Entry = Entry Int Int Int Int Int Int Int deriving (Show)

mapIncr :: (Int,Int) -> Map (Int,Int) Int -> Map (Int,Int) Int
mapIncr a m =
  Data.Map.insert a ((Data.Map.findWithDefault 0 a m)+1) m

mapCons :: (Int,Int) -> Int -> Map (Int,Int) [Int] -> Map (Int,Int) [Int]
mapCons a i m =
  Data.Map.insert a (i:(Data.Map.findWithDefault [] a m)) m

processLine l =
  let ((_:id):l':[]) = splitOn " @ " l in
  let (xy:wh:[]) = splitOn ": " l' in
  let (x:y:[]) = splitOn "," xy in
  let (w:h:[]) = splitOn "x" wh in
  Entry (read id) (read x) (read y) (read w) (read h) (read w) (read h)

buildMap :: [Entry] -> Map (Int,Int) [Int] -> Map (Int,Int) [Int]
buildMap [] m = m
buildMap ((Entry i x y w h 1 1):ls) m = buildMap ls (mapCons (x,y) i m)
buildMap ((Entry i x y w h 1 ch):ls) m =
  buildMap ((Entry i x y w h w (ch-1)):ls) (mapCons (x,y+ch-1) i m)
buildMap ((Entry i x y w h cw ch):ls) m =
  buildMap ((Entry i x y w h (cw-1) ch):ls) (mapCons (x+cw-1,y+ch-1) i m)

deleteAll :: [Int] -> Set Int -> Set Int
deleteAll [] s = s
deleteAll (id:ids) s = deleteAll ids (Data.Set.delete id s)

extractMap :: [(Int,Int)] -> Map (Int,Int) [Int] -> Set Int -> Set Int
extractMap [] m s = s
extractMap (k:ks) m s =
  let ids = Data.Map.findWithDefault [] k m in
  if length ids > 1 then extractMap ks m (deleteAll ids s) else extractMap ks m s

main = do
  input <- getContents
  let ls = lines input
  let ls' = Prelude.map processLine ls
  let ids = Data.Set.fromList (Prelude.map (\(Entry i x y w s cw ch) -> i) ls')
  let m = buildMap ls' (Data.Map.empty)
  -- print ls'
  -- print (m)
  print (extractMap (Data.Map.keys m) m ids)
