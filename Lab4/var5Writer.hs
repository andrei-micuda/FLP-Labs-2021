--- Monada Writer

newtype StringWriter a = StringWriter {runStringWriter :: (a, String)}

type M a = StringWriter a

instance (Show a) => Show (StringWriter a) where
  show m = let (val, output) = runStringWriter m in "Output: " ++ output ++ " Value: " ++ show val

instance Monad StringWriter where
  return x = StringWriter (x, "")
  mx >>= f =
    let (x, out1) = runStringWriter mx
        (y, out2) = runStringWriter (f x)
     in StringWriter (y, out1 ++ out2)

instance Applicative StringWriter where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)

instance Functor StringWriter where
  fmap f ma = pure f <*> ma

tell :: String -> StringWriter ()
tell s = StringWriter ((), s)

--- Limbajul si  Interpretorul

showM :: Show a => M a -> String
showM = show

type Name = String

data Term
  = Var Name
  | Con Integer
  | Term :+: Term
  | Lam Name Term
  | App Term Term
  | Out Term
  deriving (Show)

pgm :: Term
pgm =
  App
    ( Lam
        "y"
        ( App
            ( App
                ( Lam
                    "f"
                    ( Lam
                        "y"
                        (App (Var "f") (Var "y"))
                    )
                )
                ( Lam
                    "x"
                    (Var "x" :+: Var "y")
                )
            )
            (Con 3)
        )
    )
    (Con 4)

data Value
  = Num Integer
  | Fun (Value -> M Value)
  | Wrong

instance Show Value where
  show (Num x) = show x
  show (Fun _) = "<function>"
  show Wrong = "<wrong>"

type Environment = [(Name, Value)]

interp :: Term -> Environment -> M Value
interp (Var n) env = case (lookup n env) of
  Nothing -> return Wrong
  Just v -> return v
interp (Con i) _ = return (Num i)
interp (t1 :+: t2) env = do
  v1 <- interp t1 env
  v2 <- interp t2 env
  add v1 v2
interp (Lam var term) env = return $ Fun (\val -> interp term ((var, val) : env))
interp (App t1 t2) env = do
  v1 <- interp t1 env
  v2 <- interp t2 env
  app v1 v2
interp (Out t) env = do
  v <- interp t env
  tell $ show v ++ "; "
  return v

add :: Value -> Value -> M Value
add (Num x) (Num y) = return $ Num (x + y)
add _ _ = return Wrong

app :: Value -> Value -> M Value
app (Fun f) v = f v
app _ _ = return Wrong

test :: Term -> String
test t = showM $ interp t []

pgm1 :: Term
pgm1 =
  App
    (Lam "x" ((Var "x") :+: (Var "x")))
    ((Con 10) :+: (Con 11))

-- test pgm
-- test pgm1