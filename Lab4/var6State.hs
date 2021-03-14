--- Monada State

newtype IntState a = IntState {runIntState :: Integer -> (a, Integer)}

type M a = IntState a

instance (Show a) => Show (IntState a) where
  show m =
    let (val, state) = runIntState m 0
     in "Value: " ++ show val ++ "; Count: " ++ show state

instance Monad IntState where
  return x = IntState (\i -> (x, i))
  mx >>= f = IntState g
    where
      g state =
        let (a, intermState) = runIntState mx state
            (b, finState) = runIntState (f a) intermState
         in (b, finState)

instance Applicative IntState where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)

instance Functor IntState where
  fmap f ma = pure f <*> ma

modify :: (Integer -> Integer) -> IntState ()
modify f = IntState (\state -> ((), f state))

tickS :: IntState ()
tickS = IntState (\stare -> ((), stare + 1))

get :: IntState Integer
get = IntState (\stare -> (stare, stare))

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
  | Count
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
interp Count _ = do
  count <- get
  return $ Num count

add :: Value -> Value -> M Value
add (Num x) (Num y) = do
  tickS
  return $ Num (x + y)
add _ _ = return Wrong

app :: Value -> Value -> M Value
app (Fun f) v = do
  tickS
  f v
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