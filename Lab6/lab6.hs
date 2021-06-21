module Checker where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import SIMPLE

data Type = TInt | TBool
  deriving (Eq)

instance Show Type where
  show TInt = "int"
  show TBool = "bool"

type CheckerState = Map Name Type

emptyCheckerState :: CheckerState
emptyCheckerState = Map.empty

newtype EReader a = EReader {runEReader :: CheckerState -> (Either String a)}

throwError :: String -> EReader a
throwError e = EReader (\_ -> (Left e))

instance Monad EReader where
  return a = EReader (\env -> Right a)
  act >>= k = EReader f
    where
      f env = case (runEReader act env) of
        Left s -> Left s
        Right va -> runEReader (k va) env

instance Functor EReader where
  fmap f ma = do
    a <- ma
    return (f a)

instance Applicative EReader where
  pure = return
  mf <*> ma = do f <- mf; a <- ma; return (f a)

askEReader :: EReader CheckerState
askEReader = EReader (\env -> Right env)

localEReader :: (CheckerState -> CheckerState) -> EReader a -> EReader a
localEReader f ma = EReader (\env -> (runEReader ma) (f env))

type M = EReader

expect :: (Show t, Eq t, Show e) => t -> t -> e -> M ()
expect tExpect tActual e =
  if (tExpect /= tActual)
    then
      ( throwError $
          "Type mismatch. Expected " <> show tExpect <> " but got " <> show tActual
            <> " for "
            <> show e
      )
    else (return ())

lookupM :: Name -> M Type
lookupM x = do
  env <- askEReader
  case (Map.lookup x env) of
    Nothing -> throwError $ "Variable " <> x <> "not declared"
    Just t -> return t

checkExp :: Exp -> M Type
checkExp (Id var) = lookupM var
checkExp (I _) = return TInt
checkExp (B _) = return TBool
checkExp (UMin exp) = checkExp exp
checkExp (BinA _ a b) = do
  -- t_a <- checkExp a
  -- case t_a of
  --   TBool -> throwError "Expression is of type bool"
  -- t_b <- checkExp b
  -- case t_b of
  --   TBool -> throwError "Expression is of type bool"
  -- return TInt
  t_a <- checkExp a
  expect TInt t_a a
  t_b <- checkExp b
  expect TInt t_b b
  return TInt
checkExp (BinC _ a b) = do
  t_a <- checkExp a
  expect TInt t_a a
  t_b <- checkExp b
  expect TInt t_b b
  return TInt
checkExp (BinE _ a b) = do
  t_a <- checkExp a
  expect TInt t_a a
  t_b <- checkExp b
  expect TInt t_b b
  return TInt
checkExp (BinL _ a b) = do
  t_a <- checkExp a
  expect TBool t_a a
  t_b <- checkExp b
  expect TBool t_b b
  return TBool
checkExp (Not e) = do
  t_e <- checkExp e
  expect TBool t_e e
  return TBool

checkStmt :: Stmt -> M ()
checkStmt (Asgn var exp) = do
  t_var <- lookupM var
  t_exp <- checkExp exp
  expect t_var t_exp var
checkStmt (If exp thn els) = do
  t_exp <- checkExp exp
  expect TBool t_exp exp
  checkStmt thn
  checkStmt els
checkStmt (Read _ var) = do
  t_var <- lookupM var
  expect TInt t_var var
checkStmt (Print _ exp) = do
  t_exp <- checkExp exp
  expect TInt t_exp exp
checkStmt (While exp stmt) = do
  t_exp <- checkExp exp
  expect TBool t_exp exp
  checkStmt stmt
checkStmt (Block ss) = checkBlock ss
checkStmt (Decl _ _) = return ()

checkBlock :: [Stmt] -> M ()
checkBlock [stmt] = checkStmt stmt
checkBlock (s : ss) = do
  checkStmt s
  checkBlock ss

checkPgm :: [Stmt] -> Bool
checkPgm pgm =
  case (runEReader (checkBlock pgm)) emptyCheckerState of
    Left err -> error err
    Right _ -> True