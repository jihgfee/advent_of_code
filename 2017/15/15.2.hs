import Numeric (showIntAtBase);
import Data.Char (intToDigit);


maxVal = 2147483647

generate n i = mod (i * n) maxVal
generate1 = generate 16807
generate2 = generate 48271

toBinary i = showIntAtBase 2 intToDigit i ""

--padZeroes' 0 ss = ss
--padZeroes' n ss = '0' : padZeroes' (n-1) ss
padZeroes ss = if (length ss) > 16
                 then drop ((length ss)-16) ss
                 else (take (16-(length ss)) (repeat '0')) ++ ss   

--padZeroes' (32-(length ss)) ss

checkMatchS s1 s2 = if s1 == s2 then 1 else 0
checkMatch v1 v2 = checkMatchS (padZeroes (toBinary v1)) (padZeroes (toBinary v2))

checkMatches [] v2s = 0
checkMatches v1s [] = 0
checkMatches (v1:v1s) (v2:v2s) = checkMatch v1 v2 + checkMatches v1s v2s

verifyVal d i = mod i d == 0
verifyVal1 = verifyVal 4
verifyVal2 = verifyVal 8

generateAll _ _ 0 _ = []
generateAll vf gen n v = if vf v
                       then v:(generateAll vf gen (n-1) (gen v))
                       else (generateAll vf gen n (gen v))

generateAll1 = generateAll verifyVal1 generate1
generateAll2 = generateAll verifyVal2 generate2

run n v1 v2 = checkMatches (generateAll1 n (generate1 v1)) (generateAll2 n (generate2 v2))

main = do
  input <- getContents
  let ls = map words (lines (input))
  let (g1,g2) = (read (head (drop 4 (head ls))), read (head (drop 4 (head (tail ls)))))
  --print (padZeroes (toBinary (generate1 65)))
  print (run 5000000 g1 g2)
