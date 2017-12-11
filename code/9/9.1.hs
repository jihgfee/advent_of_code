
filterNegs [] = ""
filterNegs ('!':c:s) = filterNegs s
filterNegs (c:s) = c : (filterNegs s)

filterGarbage b [] = ""
filterGarbage False ('<':s) = filterGarbage True s
filterGarbage False (c:s) = c : filterGarbage False s
filterGarbage True ('>':s) = filterGarbage False s
filterGarbage True (c:s) = filterGarbage True s


sumGroups i [] = 0
sumGroups i ('}':s) = sumGroups (i-1) s
sumGroups i ('{':s) = i + sumGroups (i+1) s
sumGroups i (c:s) = sumGroups i s

main = do
  input <- getContents
  print (sumGroups 1 (filterGarbage False (filterNegs input)))
