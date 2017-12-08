printNodes [] = return ()
printNodes (b:bs) = do
  printNode b
  printNodes bs

--printNode (Leaf a) = do print a
printNode (Branch a bs) = do
  print a
  printNodes bs
