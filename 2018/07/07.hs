import Data.List.Split
import Data.Map
import Data.Set
import Data.Sort
import Data.String


splitOnAll :: [String] -> String -> [String]
splitOnAll [] l = [l]
splitOnAll (s:ss) l =
  let ls = splitOn s l in
  Prelude.foldr (++) [] (Prelude.map (splitOnAll ss) ls)

processLine :: String -> (Char, Char)
processLine s =
  let (_:(a:_):(b:_):_) = splitOnAll (["Step "," must be finished before step "," can begin."]) s in
  (a, b)

singletonOrAppend d Nothing = Just ([d])
singletonOrAppend d (Just ds) = Just (d:ds)
  
mapAdd m k v =
  Data.Map.alter (singletonOrAppend v) k m

buildMaps [] m m' = (m, m')
buildMaps ((a,b):ds) m m' =
  buildMaps ds (mapAdd m a b) (mapAdd m' b a)

findFringe es ds =
  sort (Data.Set.toList ((Data.Set.difference
        (Data.Set.fromList (Data.Map.keys es))
        (Data.Set.fromList (Data.Map.keys ds)))))

insertFringe fs fs' = sort (fs ++ fs')

removeFringe i []  = []
removeFringe i (f:fs) | i == f = fs
removeFringe i (f:fs)  = f:(removeFringe i fs)

removeDeps :: Char -> [Char] -> Map Char [Char] -> Map Char [Char] -> [Char] -> (Map Char [Char], [Char])
removeDeps f [] ds es fs = (ds, fs)
removeDeps f (e:es') ds es fs =
  let d = Data.Map.findWithDefault [] e ds in
  let d' = removeFringe f d in
  let fs' = if (d' == []) then (e:fs) else fs in
  removeDeps f es' (Data.Map.insert e d' ds) es fs'

processDeps :: [Char] -> Map Char [Char] -> Map Char [Char] -> [Char]
processDeps [] ds es = []  
processDeps (f:fs) ds es =
  let e = Data.Map.findWithDefault [] f es in
  let (ds', fs') = removeDeps f e ds es [] in
  f : (processDeps (insertFringe fs fs') ds' es)
  
main = do
  input <- getContents
  let ls = Prelude.map (processLine) (lines input)
  print ls
  let (es, ds) = buildMaps ls (Data.Map.empty) (Data.Map.empty)
  let fringe = findFringe es ds
  print $ processDeps fringe ds es
