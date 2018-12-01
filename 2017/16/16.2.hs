import Data.List.Split;
import Data.Char (isSpace)
rstrip = reverse . dropWhile isSpace . reverse

dancerCount = 16

nextLetter c
    | c == 'Z' = 'A'
    | otherwise = succ c

mkDancers' n 0 = []
mkDancers' c i = (c:[]) : mkDancers' (nextLetter c) (i-1)
dancers = mkDancers' 'a' dancerCount

data Instr = Spin Int | Exchange Int Int | Partner String String

eval ('s':ss) = Spin (read ss)
eval ('x':ss) = let ss' = splitOn "/" ss in 
                let (i,j) = ((read (head ss')), (read (head (tail ss')))) in
                if i < j then Exchange i j
                else Exchange j i
eval ('p':ss) = let ss' = splitOn "/" ss in Partner (head ss') (rstrip (head (tail ss')))

evalAll [] = []
evalAll (s:ss) = eval s : evalAll ss 

executeExch' a j (l:ls) = if j == 0 then (l, a:ls)
                          else let (b,ls') = executeExch' a (j-1) ls in (b, l:ls')
executeExch i j (l:ls) = if i == 0 then let (b, ls') = executeExch' l (j-1) ls in (b:ls') 
                         else (l : executeExch (i-1) (j-1) ls)

executePart a b [] = []
executePart a b (l:ls) = (if l == a then b else if l == b then a else l) : executePart a b ls
execute (Spin x) l = drop (dancerCount-x) l ++ take (dancerCount-x) l
execute (Exchange i j) l = executeExch i j l
execute (Partner a b) l = executePart a b l

executeAll [] l = l
executeAll (instr:instrs) l = executeAll (instrs) (execute instr l)

buildPermutations instrs l ls = let l' = executeAll instrs l in
                                if elem l' ls then ls
                                else buildPermutations instrs l' (l':ls)

repeatExecuteAll 0 instrs l = l
repeatExecuteAll i instrs l = repeatExecuteAll (i-1) instrs (executeAll instrs l)

stringFlatten [] = []
stringFlatten (s:ss) = s ++ stringFlatten ss

elemAt (l:ls) 0 = l
elemAt (l:ls) i = elemAt ls (i-1)

findHitAt n instrs l = let pms = reverse (buildPermutations instrs l [l]) in
                       let idx = mod n (length pms) in
                       elemAt pms idx

main = do
  input <- getContents
  let l = evalAll (splitOn "," input)
  let l' = dancers
  print (stringFlatten (findHitAt 1000000000 l l'))


