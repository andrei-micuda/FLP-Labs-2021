--- Monada Writer

newtype WriterLS a = Writer {runWriter :: (a, [String])}

instance Monad WriterLS where
  return va = Writer (va, [])
  ma >>= k =
    let (va, log1) = runWriter ma
        (vb, log2) = runWriter (k va)
     in Writer (vb, log1 ++ log2)

instance Applicative WriterLS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)

instance Functor WriterLS where
  fmap f ma = pure f <*> ma

tell :: String -> WriterLS ()
tell log = Writer ((), [log])

logIncrement :: Int -> WriterLS Int
logIncrement x = Writer (x + 1, ["increment: " ++ show x])

logIncrementN :: Int -> Int -> WriterLS Int
logIncrementN x n =
  if n == 0
    then Writer (x, [])
    else do
      y <- logIncrement x
      logIncrementN y (n - 1)

isPos :: Int -> WriterLS Bool
isPos x = if (x >= 0) then (Writer (True, ["poz"])) else (Writer (False, ["neg"]))

mapWriterLS :: (a -> WriterLS b) -> [a] -> WriterLS [b]
mapWriterLS f xs = func $ map f xs
  where
    func :: [WriterLS b] -> WriterLS [b]
    func [] = return []
    func (mx : mxs) = do
      x <- mx
      xs <- func mxs
      return (x : xs)
