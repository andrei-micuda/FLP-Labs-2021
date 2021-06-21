module Lab7 where

import AST
import Checker (checkPgm)
import Control.Monad.State.Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Parse (loadProgramFromFile)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)

data Value = IVal Integer | BVal Bool
  deriving (Show, Eq)

data ImpState = ImpState
  { env :: Map String Int,
    store :: Map Int Value,
    nextLoc :: Int
  }
  deriving (Show)

compose :: Ord b => Map b c -> Map a b -> Map a c
compose bc ab
  | null bc = Map.empty
  | otherwise = Map.mapMaybe (bc Map.!?) ab

showImpState :: ImpState -> String
showImpState st =
  "Final state: " <> show (compose (store st) (env st))

emptyState :: ImpState
emptyState = ImpState Map.empty Map.empty 0

type M = StateT ImpState IO

runM :: M a -> IO (a, ImpState)
runM m = runStateT m emptyState

lookupM :: String -> M Value
lookupM x = do
  Just l <- Map.lookup x <$> gets env
  Just v <- Map.lookup l <$> gets store
  return v

updateM :: String -> Value -> M ()
updateM x v = do
  Just l <- Map.lookup x <$> gets env
  st <- gets store
  let st' = Map.insert l v st
  modify' (\s -> s {store = st'})

aop :: BinAop -> Integer -> Integer -> Integer
aop Add = (+)
aop Mul = (*)
aop Sub = (-)
aop Div = div
aop Mod = mod

cop :: BinCop -> Integer -> Integer -> Bool
cop Lt = (<)
cop Lte = (<=)
cop Gt = (>)
cop Gte = (>=)

eop :: BinEop -> Value -> Value -> Bool
eop Eq v1 v2 = v1 == v2
eop Neq v1 v2 = v1 /= v2

lop :: BinLop -> Bool -> Bool -> Bool
lop And = (&&)
lop Or = (||)

evalExp :: Exp -> M Value
evalExp (Id x) = lookupM x
evalExp (I int) = return (IVal int)
evalExp (B bool) = return (BVal bool)
evalExp (UMin exp) = do
  IVal int <- evalExp exp
  return (IVal (- int))
evalExp (BinA op e1 e2) = do
  IVal i1 <- evalExp e1
  IVal i2 <- evalExp e2
  return (IVal $ aop op i1 i2)
evalExp (BinC op e1 e2) = do
  IVal i1 <- evalExp e1
  IVal i2 <- evalExp e2
  return (BVal $ cop op i1 i2)
evalExp (BinE op e1 e2) = do
  v1 <- evalExp e1
  v2 <- evalExp e2
  return (BVal $ eop op v1 v2)
evalExp (BinL op e1 e2) = do
  BVal b1 <- evalExp e1
  BVal b2 <- evalExp e2
  return (BVal $ lop op b1 b2)
evalExp (Not e) = do
  BVal b <- evalExp e
  return (BVal $ not b)
evalExp e = error $ "Evaluation for '" <> show e <> "' not yet defined."

evalStmt :: Stmt -> M ()
evalStmt (Asgn x e) = do
  v <- evalExp e
  updateM x v
evalStmt (Inc (Id x)) = do
  IVal int <- evalExp (Id x)
  let newVal = IVal $ int + 1
  updateM x newVal
evalStmt (If e s1 s2) = do
  BVal b <- evalExp e
  if b then evalStmt s1 else evalStmt s2
evalStmt (While e s) = do
  BVal b <- evalExp e
  if b
    then do
      evalStmt s
      evalStmt (While e s)
    else return ()
evalStmt (Read s x) = do
  i <- liftIO (putStr s >> hFlush stdout >> readLn)
  evalStmt (Asgn x (I i))
evalStmt (Print str exp) = do
  IVal int <- evalExp exp
  liftIO $ putStrLn $ str ++ " " ++ show int
evalStmt (Decl x e) = do
  v <- evalExp e
  modify' (declare v)
  where
    declare v st = ImpState env' store' nextLoc'
      where
        l = nextLoc st
        nextLoc' = 1 + nextLoc st
        store' = Map.insert l v (store st)
        env' = Map.insert x l (env st)
evalStmt (Block sts) = do
  oldEnv <- gets env
  mapM_ evalStmt sts
  modify' (\s -> s {env = oldEnv})

evalPgm :: [Stmt] -> IO ((), ImpState)
evalPgm sts = runM $ mapM_ evalStmt sts

main :: IO ()
main = do
  args <- getArgs
  when (null args) (error "Need file to run")
  let (file : _) = args
  pgm <- loadProgramFromFile file
  checkPgm pgm
  (_, st) <- evalPgm pgm
  putStrLn $ showImpState st