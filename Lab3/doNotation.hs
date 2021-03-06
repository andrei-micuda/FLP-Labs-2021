-- 4
cartesianProduct xs ys = xs >>= (\x -> ys >>= \y -> return (x, y))

cartesianProductDo xs ys = do
  x <- xs
  y <- ys
  return (x, y)

prod f xs ys = [f x y | x <- xs, y <- ys]

prodDo f xs ys = do
  x <- xs
  y <- ys
  return (f x y)

myGetLine :: IO String
myGetLine =
  getChar >>= \x ->
    if x == '\n'
      then return []
      else myGetLine >>= \xs -> return (x : xs)

myGetLineDo :: IO String
myGetLineDo = do
  x <- getChar
  if x == '\n'
    then return []
    else do
      xs <- myGetLine
      return (x : xs)

prelNo noin = sqrt noin

ioNumber = do
  noin <- readLn :: IO Float
  putStrLn $ "Intrare\n" ++ (show noin)
  let noout = prelNo noin
  putStrLn $ "Iesire"
  print noout

-- 5
ioNumberSecv =
  (readLn :: IO Float) >>= \noin ->
    putStrLn ("Intrare\n" ++ (show noin))
      >> let noout = prelNo noin
          in (putStrLn "Iesire") >> (print noout)