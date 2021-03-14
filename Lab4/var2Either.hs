--- Monada Either
type M = Either String

--- Limbajul si  Interpretorul

showM :: Show a => M a -> String
showM (Right v) = show v
showM (Left s) = s

type Name = String

data Term
  = Var Name
  | Con Integer
  | Term :+: Term
  | Lam Name Term
  | App Term Term
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

instance Show Value where
  show (Num x) = show x
  show (Fun _) = "<function>"

type Environment = [(Name, Value)]

interp :: Term -> Environment -> M Value
interp (Var n) env = case (lookup n env) of
  Just v -> Right v
  Nothing -> Left $ "unbound variable: " ++ n
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

add :: Value -> Value -> M Value
add (Num x) (Num y) = return $ Num (x + y)
add x y = Left $ "should be numbers: " ++ show x ++ " " ++ show y

app :: Value -> Value -> M Value
app (Fun f) v = f v
app f _ = Left $ "should be function: " ++ show f

test :: Term -> String
test t = showM $ interp t []

pgm1 :: Term
pgm1 =
  App
    (Lam "x" ((Var "x") :+: (Var "x")))
    ((Con 10) :+: (Con 11))

-- test pgm
-- test pgm1