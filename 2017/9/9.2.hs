
filterNegs [] = ""
filterNegs ('!':c:s) = filterNegs s
filterNegs (c:s) = c : (filterNegs s)

countGarbage b [] = 0
countGarbage False ('<':s) = countGarbage True s
countGarbage False (c:s) = countGarbage False s
countGarbage True ('>':s) = countGarbage False s
countGarbage True (c:s) = 1 + countGarbage True s

main = do
  input <- getContents
  print (countGarbage False (filterNegs input))
