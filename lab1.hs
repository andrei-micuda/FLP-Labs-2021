data Prog = On Instr

data Instr = Off | Expr :> Instr

data Expr = Mem | V Int | Expr :+ Expr

type Env = Int -- valoarea celulei de memorie

type DomProg = [Int]

type DomInstr = Env -> [Int]

type DomExpr = Env -> Int

prog :: Prog -> DomProg
prog (On instr) = stmt instr 0

stmt :: Instr -> DomInstr
stmt Off _ = []
stmt (ex :> instr) m =
  let val = expr ex m in val : (stmt instr val)

expr :: Expr -> DomExpr
expr Mem m = m
expr (V i) _ = i
expr (expr1 :+ expr2) m = expr expr1 m + expr expr2 m

p1 = On ((V 3) :> ((Mem :+ (V 5)) :> Off))

type Name = String

data Hask
  = HTrue
  | HFalse
  | HLit Int
  | HIf Hask Hask Hask
  | Hask :==: Hask
  | Hask :+: Hask
  | HVar Name
  | HLam Name Hask
  | Hask :$: Hask
  deriving (Read, Show)

infix 4 :==:

infixl 6 :+:

infixl 9 :$:

data Value
  = VBool Bool
  | VInt Int
  | VFun (Value -> Value)
  | VError -- pentru reprezentarea erorilor

type HEnv = [(Name, Value)]

type DomHask = HEnv -> Value

instance Show Value where
  show (VBool b) = show b
  show (VInt i) = show i
  show (VFun func) = "func"
  show VError = "err"

instance Eq Value where
  (VBool b1) == (VBool b2) = b1 == b2
  (VInt i1) == (VInt i2) = i1 == i2
  _ == _ = error "Cannot apply equality"

hEval :: Hask -> DomHask
hEval HTrue _ = VBool True
hEval HFalse _ = VBool False
hEval (HLit i) _ = VInt i
hEval (HIf pred if' else') henv = evalIf (hEval pred if')