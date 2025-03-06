import Data.Char

processLine ('o':'n':'e':s) = '1' : processLine ('n':'e':s)
processLine ('t':'w':'o':s) = '2' : processLine ('w':'o':s)
processLine ('t':'h':'r':'e':'e':s) = '3' : processLine ('h':'r':'e':'e':s)
processLine ('f':'o':'u':'r':s) = '4' : processLine ('o':'u':'r':s)
processLine ('f':'i':'v':'e':s) = '5' : processLine ('i':'v':'e':s)
processLine ('s':'i':'x':s) = '6' : processLine ('i':'x':s)
processLine ('s':'e':'v':'e':'n':s) = '7' : processLine ('e':'v':'e':'n':s)
processLine ('e':'i':'g':'h':'t':s) = '8' : processLine ('i':'g':'h':'t':s)
processLine ('n':'i':'n':'e':s) = '9' : processLine ('i':'n':'e':s)
processLine (c:s) = c : processLine s
processLine [] = []

stringToCalibrationNumber s =
  let c1 = head s in
  let c2 = head (reverse s) in
  read (c1 : [c2])

main = do
  input <- getContents
  let allLines = lines input;
  let processedLines = map (\s -> filter isDigit s) (map processLine allLines);
  print processedLines;
  print (sum (map stringToCalibrationNumber processedLines));
  return ()
