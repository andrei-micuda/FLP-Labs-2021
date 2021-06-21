newtype MaybeWriter a = MW {getvalue :: Maybe (a, String)}

instance Monad MaybeWriter where
  return a = MW $ Just $ (a, "")
  ma >>= k = case getvalue ma of
    Nothing -> MW Nothing
    Just (a, log1) -> case getvalue (k a) of
      Nothing -> MW Nothing
      Just (b, log2) -> MW $ Just (b, log1 ++ log2)

instance Applicative MaybeWriter where
  pure = return
  mf <*> ma = do f <- mf; a <- ma; return (f a)

instance Functor MaybeWriter where
  fmap f ma = pure f <*> ma

type Name = String

data Term
  = Var Name
  | Con Integer
  | Term :+: Term
  | Term :/: Term
  | Lam Name Term
  | App Term Term
  | Out Term
  deriving (Show)

type M a = MaybeWriter a

showM :: Show a => M a -> String
showM ma = case getvalue ma of
  Nothing -> "Nothing"
  Just (a, w) -> "Output: " ++ w ++ "\nValue: " ++ show a

data Value
  = Num Integer
  | Fun (Value -> M Value)

type Environment = [(Name, Value)]

instance Show Value where
  show (Num x) = show x
  show (Fun _) = "<function>"

get :: Name -> Environment -> M Value
get x env = case [v| (y, v) <- env, x == y] of
    (v:_) -> return v
    _ -> MW Nothing

interp :: Term -> Environment -> M Value
interp (Var x) env = get x env
interp (Con i) _ = return $ Num i
interp (t1 :+: t2) env = do
    v1 <- interp t1 env
    v2 <- interp t2 env
    add v1 v2
interp (t1 :/: t2) env = do
    v1 <- interp t1 env
    v2 <- interp t2 env
    Main.div v1 v2
interp (Lam x e) env = return $ Fun $ \v -> interp e ((x, v):env)
interp (App t1 t2) env = do
    v1 <- interp t1 env
    v2 <- interp t2 env
    app v1 v2
interp (Out t) env = do
    v <- interp t env
    tell (show v ++ "; ")
    return v

add :: Value -> Value -> M Value
add (Num i1) (Num i2) = return $ Num $ i1 + i2
add _ _ = MW Nothing

div :: Value -> Value -> M Value
div _ (Num 0) = MW Nothing 
div (Num i1) (Num i2) = return $ Num $ i1 `Prelude.div` i2
div _ _ = MW Nothing

app :: Value -> Value -> M Value
app (Fun f) v = f v
app _ _ = MW Nothing

tell :: String -> MaybeWriter ()
tell log = MW $ Just ((), log)

test :: Term -> String
test t = showM $ interp t []

pgm :: Term
pgm = App
    (Lam "x" ((Var "x") :+: (Var "x")))
    ((Out (Con 10)) :+: (Out (Con 11)))

pgmW :: Term
pgmW = App (Var "y") (Lam "y" (Out (Con 3)))

pgm2 :: Term
pgm2 = App
    (Lam "x" ((Var "x") :+: (Var "x")))
    ((Con 10) :/:  (Out (Con 2)))

pgmW2 :: Term
pgmW2 = App
    (Lam "x" ((Var "x") :+: (Var "x")))
    ((Con 10) :/:  (Out (Con 0)))