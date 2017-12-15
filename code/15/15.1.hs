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

run' 0 v1 v2 = 0
run' n v1 v2 = checkMatch v1 v2 + run' (n-1) (generate1 v1) (generate2 v2)
run n v1 v2 = run' n (generate1 v1) (generate2 v2)

main = do
  input <- getContents
  let ls = map words (lines (input))
  let (g1,g2) = (read (head (drop 4 (head ls))), read (head (drop 4 (head (tail ls)))))
  --print (padZeroes (toBinary (generate1 65)))
  print (run 40000000 g1 g2)
