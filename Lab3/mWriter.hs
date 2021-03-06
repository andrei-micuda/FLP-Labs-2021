--- Monada Writer

newtype WriterS a = Writer {runWriter :: (a, String)}

instance Monad WriterS where
  return va = Writer (va, "")
  ma >>= k =
    let (va, log1) = runWriter ma
        (vb, log2) = runWriter (k va)
     in Writer (vb, log1 ++ log2)

instance Applicative WriterS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)

instance Functor WriterS where
  fmap f ma = pure f <*> ma

tell :: String -> WriterS ()
tell log = Writer ((), log)

-- 1.1
logIncrement :: Int -> WriterS Int
logIncrement x = do
  tell $ "increment: " ++ show x ++ "\n"
  return (x + 1)

logIncrement2 :: Int -> WriterS Int
logIncrement2 x = do
  y <- logIncrement x
  logIncrement y

-- 1.2
logIncrementN :: Int -> Int -> WriterS Int
logIncrementN x n =
  if n == 0
    then Writer (x, "")
    else do
      y <- logIncrement x
      logIncrementN y (n -1)

-- 5
isPos :: Int -> WriterS Bool
isPos x = if (x >= 0) then (Writer (True, "poz")) else (Writer (False, "neg"))

mapWriterS :: (a -> WriterS b) -> [a] -> WriterS [b]
mapWriterS f xs = func $ map f xs
  where
    func :: [WriterS b] -> WriterS [b]
    func [] = return []
    func (mx : mxs) = do
      x <- mx
      xs <- func mxs
      return (x : xs)