{- Monada Maybe este definita in GHC.Base

instance Monad Maybe where
  return = Just
  Just va  >>= k   = k va
  Nothing >>= _   = Nothing

instance Applicative Maybe where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)

instance Functor Maybe where
  fmap f ma = pure f <*> ma
-}

(<=<) :: (a -> Maybe b) -> (c -> Maybe a) -> c -> Maybe b
f <=< g = (\x -> g x >>= f)

-- 1.2
asoc :: (Int -> Maybe Int) -> (Int -> Maybe Int) -> (Int -> Maybe Int) -> Int -> Bool
asoc f g h x = (f <=< (g <=< h)) x == ((f <=< g) <=< h) x

asocTest = asoc (\x -> if even x then Just (x `div` 2) else Nothing) (\x -> Nothing) (\x -> Just x)

-- 2
pos :: Int -> Bool
pos x = if (x >= 0) then True else False

foo :: Maybe Int -> Maybe Bool
foo mx = mx >>= (\x -> Just (pos x))

-- 2.2
fooDo :: Maybe Int -> Maybe Bool
fooDo mx = do
  x <- mx
  return $ pos x

-- 3
addM :: Maybe Int -> Maybe Int -> Maybe Int
addM mx my = undefined

-- 3.1
addMSabloane :: Maybe Int -> Maybe Int -> Maybe Int
addMSabloane (Just x) (Just y) = Just (x + y)
addMSabloane _ _ = Nothing

-- 3.2
addMDo :: Maybe Int -> Maybe Int -> Maybe Int
addMDo mx my = do
  x <- mx
  y <- my
  return (x + y)

-- 3.3
addMTest :: Maybe Int -> Maybe Int -> Bool
addMTest mx my = (addMSabloane mx my) == (addMDo mx my)

-- 5
isPos :: Int -> Maybe Bool
isPos x = if (x >= 0) then (Just True) else Nothing

mapMaybe :: (a -> Maybe b) -> [a] -> Maybe [b]
mapMaybe f xs = func $ map f xs
  where
    func :: [Maybe b] -> Maybe [b]
    func [] = return []
    func (mx : mxs) = do
      x <- mx
      xs <- func mxs
      return (x : xs)