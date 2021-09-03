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

processEntry :: [Entry] -> [(Int,Int)]
processEntry [] = []
processEntry ((Entry _ (Begin _)):es) = []
processEntry ((Entry dt Sleeps):(Entry dt' Wakes):es) =
  (minute dt, minute dt'):processEntry es

processEntries :: [Entry] -> [Elf]
processEntries [] = []
processEntries ((Entry dt (Begin id)):es) =
  let intervals = processEntry es in
  (Elf id intervals) : processEntries (Prelude.drop ((length intervals)*2) es)

elfAppend e Nothing = Just e
elfAppend (Elf id is) (Just (Elf id' is')) = Just (Elf id (is'++is))

elfMapAppend [] m = m
elfMapAppend ((Elf id is):es) m =
  elfMapAppend es (alter (elfAppend (Elf id is)) id m)

chooseElf :: [Elf] -> Maybe Elf -> (Elf -> Int) -> Elf
chooseElf [] (Just e) _ = e
chooseElf (e:es) Nothing c = chooseElf es (Just e) c
chooseElf (e1:es) (Just e2) c =
  chooseElf es (if (c e1 > c e2) then Just e1 else Just e2) c

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
  mapMax (buildOverlaps is (Data.Map.empty))

sumIntervals' :: [(Int,Int)] -> Int
sumIntervals' [] = 0
sumIntervals' ((s,e):is) = ((e-s)+1)+(sumIntervals' is)
sumIntervals (Elf id is) = sumIntervals' is

main = do
  input <- getContents
  let ls = lines input
  let splitters = [" ","[","]","#","-",":"]
  let splits = Prelude.map (splitOnAll splitters) ls
  let entries = sort (Prelude.map (processLine) splits)
  let elves = processEntries entries
  let extractVal = sumIntervals
  -- let extractVal = (snd <$> highestOverlap)
  let squashed_elves = elfMapAppend elves (Data.Map.empty)
  -- let squashed_elves = Prelude.foldr (\(Elf id is) m -> alter (elfAppend (Elf id is) id m)) Data.Map.empty elves
  let (Elf id is) = chooseElf (Prelude.map (snd) (toAscList (squashed_elves))) Nothing extractVal
  let maxElf = (Elf id is)
  let (overlapMin, value) = highestOverlap maxElf
  print (id * overlapMin)
