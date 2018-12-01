import Data.Map
import Data.Maybe 
data Node a = Branch a ([Node a])

invertMap m = Data.Map.mapWithKey (\k v -> (v,k)) m

trimChars = [',', '(',')']
trim [] = []
trim (c:cs) = if elem c trimChars then trim cs else c:(trim cs)

lineToInput (n:[w]) = (n,read (trim w),[])
lineToInput (n:w:l) = (n,read (trim w),Prelude.map trim (tail l)) 

linesToMap [] m = m
linesToMap ((n,w,ns):ls) m =
  linesToMap ls (insert n (w,ns) m)

linesToInverseMap [] m = m
linesToInverseMap ((n,w,ns):ls) m =
  linesToInverseMap ls (Prelude.foldr (\n' m -> insert n' n m) m ns)

treeHead' a m =
  if (member a m)
    then treeHead' (findWithDefault "" a m) m
    else a

treeHead l =
  let m = linesToInverseMap l empty in
  treeHead' (fst (elemAt 0 m)) m

mkBranch Nothing m = Branch (-1) [] 
mkBranch (Just (w,ns)) m = Branch w (Prelude.map (\n -> mkTree n m) ns)

--mkTree :: string -> Map string (Int,[string]) -> Node Int 
mkTree n m = mkBranch (Data.Map.lookup n m) m

updateWeights (Branch w []) = Branch (w,w) []
updateWeights (Branch w ns) =
  let ns' = Prelude.map updateWeights ns in
  let sum = Prelude.foldr (\(Branch (w',sum') ns'') a -> a+sum') w ns' in
  Branch (w, sum) ns'

findCulbrit (Branch w []) = Nothing
findCulbrit (Branch (w,sum) ns) =
  let res = Prelude.foldr (\n m -> if (isJust m) then m else findCulbrit n) Nothing ns in
  if isJust res
    then res
    else let m = Prelude.foldr (\(Branch (w',sum') ns') m -> if member sum' m then adjust (+1) sum' m else insert sum' 1 m) empty ns in
         if size m == 1
           then Nothing
           else let invMap = invertMap m in
                let diff = ((snd(snd (findMin invMap))) - (snd (snd (findMax invMap)))) in
                --Just Î(Prelude.map (\(Branch (w',sum') ns') -> (w',sum')) ns)
                Just (fst (Prelude.foldr (\(Branch (w',sum') ns') (w'',sum'') -> if sum' == sum'' then (w',sum'') else (w'',sum'')) (-1, fst (findMax (invertMap m))) ns) + diff)
  
main = do
  input <- getContents
  let l = Prelude.map (\l -> lineToInput (words l)) (lines input)
  let h = treeHead l
  let n = mkTree h (linesToMap l empty)
  print (findCulbrit (updateWeights n))
  --let lines' = map (\l -> if size l == 2 then (hd l, )) lines
  --let n = Branch 5 ((Branch 2 (Leaf 5 : [])) : Leaf 2 : [])
  --print (lines input)
