factorize i n =
  if i > n `div` 2 then Nothing else
  if n `rem` i == 0 then Just (i,n `div` i)
  else factorize (i+2) n

main = do
  input <- getLine
  let n = read input
  -- Rule out invalid even base cases
  if n < 4 then putStrLn "impossible"
  -- Handle even numbers
  else if even n then putStrLn ("2x"++(show (n `div` 2)))
  -- Handle odd base case
  else if n == 9 then putStrLn "3x3"
  -- Rule out invalid odd base cases
  else if n < 13 then putStrLn "impossible"
  -- Handle odd numbers
  else case factorize 3 n of
         Nothing -> putStrLn ("3x3 2x"++(show ((n-9) `div` 2)))
         Just (i,j)  -> putStrLn ((show i)++"x"++(show j))
