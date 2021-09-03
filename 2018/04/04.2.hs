import Data.Dates
import Data.List.Split
import Data.Sort
import Data.Map

-- Id
data EntryInfo = Begin Int | Sleeps | Wakes deriving (Show)
-- Time,
data Entry = Entry DateTime EntryInfo deriving (Show)

instance Eq Entry where
  (Entry dt _) == (Entry dt' _) = dt == dt'
instance Ord Entry where
  (Entry dt _) `compare` (Entry dt' _) = dt `compare` dt'

splitOnAll :: [String] -> String -> [String]
splitOnAll [] l = [l]
splitOnAll (s:ss) l =
  let ls = splitOn s l in
  Prelude.foldr (++) [] (Prelude.map (splitOnAll ss) ls)
  --foldr (\(l,a) -> if (length l > 0) then (l++a) else a) [] (map (splitOnAll ss) ls)

processType :: [String] -> EntryInfo
processType ("Guard":sp:id:l) = Begin (read id)
processType ("falls":l) = Sleeps
processType ("wakes":l) = Wakes

processLine :: [String] -> Entry
processLine (sp:year:month:day:hour:minute:sp':l) =
  Entry (DateTime
          (read year)
          (read month)
          (read day)
          (read hour)
          (read minute)
          0)
        (processType l)

-- Date, Sleep Intervals
data Shift = Shift DateTime [(Int,Int)]
-- Id, Shifts
data Elf = Elf Int [(Int,Int)] deriving (Show)

processEntry :: [Entry] -> ([(Int,Int)])
processEntry [] = []
processEntry ((Entry _ (Begin _)):es) = []
processEntry ((Entry dt Sleeps):(Entry dt' Wakes):es) =
  (minute dt, minute dt'):processEntry es

processEntries :: [Entry] -> [Elf]
processEntries [] = []
processEntries ((Entry dt (Begin id)):es) =
  let intervals = processEntry es in
  (Elf id intervals) : processEntries (Prelude.drop ((length intervals)*2) es)

elfAppend e Nothing = e
elfAppend (Elf id is) (Just (Elf id' is')) = Elf id (is'++is)

elfMapAppend [] m = m
elfMapAppend ((Elf id is):es) m =
  elfMapAppend es (Data.Map.insert id (elfAppend (Elf id is) (Data.Map.lookup id m)) m)


-- squashMap' [] m m' = m'
-- squashMap' (k:ks) m m' =
--   squashMap' ks m (elfMapAppend (
-- squashMap m =
--   squashMap' (Data.map.keys m) m Data.Map.empty

-- mapToList' [] m = []
-- mapToList' (k:ks) m =  : maptoList ks m
-- mapToList m =
--   toAscList m) 
sumIntervals :: [(Int,Int)] -> Int
sumIntervals [] = 0
sumIntervals ((s,e):is) = ((e-s)+1)+(sumIntervals is)

findMaxElf :: [Elf] -> Maybe Elf -> Elf
findMaxElf [] (Just e) = e
findMaxElf (e:es) Nothing = findMaxElf es (Just e)
findMaxElf ((Elf id is):es) (Just (Elf id' is')) =
  findMaxElf es (if (snd (highestOverlap (Elf id is)) > snd (highestOverlap (Elf id' is'))) then Just (Elf id is) else Just (Elf id' is'))

mapIncr a m =
  Data.Map.insert a ((Data.Map.findWithDefault 0 a m)+1) m

buildOverlaps :: [(Int,Int)] -> Map Int Int -> Map Int Int
buildOverlaps [] m = m
buildOverlaps ((s,e):is) m =
  buildOverlaps (if (s<e) then ((s+1,e):is) else is) (mapIncr s m)

mapMax' [] m Nothing = (-1,-1)
mapMax' [] m (Just (id,val)) = (id,val)
mapMax' (k:ks) m Nothing = mapMax' ks m (Just (k, findWithDefault 0 k m))
mapMax' (k:ks) m (Just (id,val)) =
  let val' = findWithDefault 0 k m in
  mapMax' ks m (if (val' > val) then Just (k, val') else Just (id,val))
mapMax m = mapMax' (Data.Map.keys m) m Nothing

highestOverlap :: Elf -> (Int,Int)
highestOverlap (Elf id is) =
  let m = buildOverlaps is (Data.Map.empty) in
  mapMax m

main = do
  input <- getContents
  let ls = lines input
  let splitters = [" ","[","]","#","-",":"]
  let splits = Prelude.map (splitOnAll splitters) ls
  let entries = sort (Prelude.map (processLine) splits)
  let elves = processEntries entries
  let squashed_elves = elfMapAppend elves (Data.Map.empty)
  
  let (Elf id is) = findMaxElf (Prelude.map (snd) (toAscList (squashed_elves))) Nothing
  let maxElf = (Elf id is)
  print maxElf
  let (overlapMin, value) = highestOverlap maxElf
  -- print entries
--  print squashed_elves
  -- print (elfMapAppend elves (Data.Map.empty))
  -- print (buildOverlaps is (Data.Map.empty))
  print (id * overlapMin)
