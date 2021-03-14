--- Monada Reader
type Environment = [(Name, Value)]

newtype EnvReader a = Reader {runEnvReader :: Environment -> a}

type M = EnvReader

instance (Show a) => Show (EnvReader a) where
  show m = show $ runEnvReader m []

instance Monad EnvReader where
  return x = Reader (const x)
  ma >>= k = Reader f
    where
      f env =
        let a = runEnvReader ma env
         in runEnvReader (k a) env

instance Applicative EnvReader where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)

instance Functor EnvReader where
  fmap f ma = pure f <*> ma

ask :: EnvReader Environment
ask = Reader id

local :: (Environment -> Environment) -> EnvReader a -> EnvReader a
local f ma = Reader (\env -> (runEnvReader ma) (f env))

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

interp :: Term -> M Value
interp (Var n) = do
  env <- ask
  case (lookup n env) of
    Nothing -> return Wrong
    Just v -> return v
interp (Con i) = return (Num i)
interp (t1 :+: t2) = do
  v1 <- interp t1
  v2 <- interp t2
  add v1 v2
interp (Lam var term) = do
  env <- ask
  return $ Fun (\val -> local (const ((var, val) : env)) (interp term))
interp (App t1 t2) = do
  v1 <- interp t1
  v2 <- interp t2
  app v1 v2

add :: Value -> Value -> M Value
add (Num x) (Num y) = return $ Num (x + y)
add _ _ = return Wrong

app :: Value -> Value -> M Value
app (Fun f) v = f v
app _ _ = return Wrong

test :: Term -> String
test t = showM $ interp t

pgm1 :: Term
pgm1 =
  App
    (Lam "x" ((Var "x") :+: (Var "x")))
    ((Con 10) :+: (Con 11))

-- test pgm
-- test pgm1