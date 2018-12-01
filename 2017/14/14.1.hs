import KnotHash;
import Data.Char;
import Numeric; 

toBit '0' = "0000"
toBit '1' = "0001"
toBit '2' = "0010"
toBit '3' = "0011"
toBit '4' = "0100"
toBit '5' = "0101"
toBit '6' = "0110"
toBit '7' = "0111"
toBit '8' = "1000"
toBit '9' = "1001"
toBit 'a' = "1010"
toBit 'b' = "1011"
toBit 'c' = "1100"
toBit 'd' = "1101"
toBit 'e' = "1110"
toBit 'f' = "1111"

toBits [] = []
toBits (c:cs) = toBit c ++ toBits cs

rows' l 128 = []
rows' l i = KnotHash.fromInput (l++"-"++(show i)) : rows' l (i+1) 
rows l = rows' (init l) 0

countUsedLine [] = 0
countUsedLine (c:cs) = (if c == '1' then 1 else 0) + countUsedLine cs
countUsed [] = 0
countUsed (l:ls) = countUsedLine l + countUsed ls

main = do
  input <- getContents
  print (countUsed (map toBits (rows input)))
