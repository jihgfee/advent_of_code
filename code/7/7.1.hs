import Data.Map

data Node a = Leaf a | Branch a ([Node a])

printNodes [] = return ()
printNodes (b:bs) = do
  printNode b
  printNodes bs

printNode (Leaf a) = do print a
printNode (Branch a bs) = do 
  print a
  printNodes bs


trimChars = [',', '(',')']
trim [] = []
trim (c:cs) = if elem c trimChars then trim cs else c:(trim cs)

lineToInput (n:[w]) = (n,trim w,[])
lineToInput (n:w:l) = (n,trim w,Prelude.map trim (tail l)) 

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

main = do
  input <- getContents
  let l = Prelude.map (\l -> lineToInput (words l)) (lines input)
  let h = treeHead l
  print (h)
  --let lines' = map (\l -> if size l == 2 then (hd l, )) lines
  --let n = Branch 5 ((Branch 2 (Leaf 5 : [])) : Leaf 2 : [])
  --print (lines input)
