type Name = String -- variabile

-- program
-- [Name] -> variabilele globale prog -> init cu 0
-- Stmt -> o intructiune
data Pgm = Pgm [Name] Stmt deriving (Read, Show)

-- instructiune
-- ::: -> secventiere de instr
-- := -> atribuiri
data Stmt = Skip | Stmt ::: Stmt | If BExp Stmt Stmt | While BExp Stmt | Name := AExp deriving (Read, Show)

-- expresii aritmetice
data AExp = Lit Integer | AExp :+: AExp | AExp :*: AExp | Var Name deriving (Read, Show)

-- expresii boolene
data BExp = BTrue | BFalse | AExp :==: AExp | Not BExp deriving (Read, Show)

infixr 2 :::

infix 3 :=

infix 4 :==:

infixl 6 :+:

infixl 7 :*:

type Env = [(Name, Integer)]

pEval :: Pgm -> Env
pEval (Pgm vars stmt) = let env = [(var, 0) | var <- vars] in sEval stmt env

sEval :: Stmt -> Env -> Env
sEval Skip env = env
sEval (stmt1 ::: stmt2) env =
  let env' = sEval stmt1 env
   in sEval stmt2 env'
sEval (If bexp stmt_true stmt_false) env = if bEval bexp env then sEval stmt_true env else sEval stmt_false env
sEval (While bexp stmt) env =
  if bEval bexp env
    then
      let env' = sEval stmt env
       in sEval (While bexp stmt) env'
    else env
sEval (var := aexp) env = assign var (aEval aexp env) env
  where
    assign :: Name -> Integer -> Env -> Env
    assign var val [] = [(var, val)]
    assign var val ((var', val') : env')
      | var' == var = (var, val) : env'
      | otherwise = (var', val') : assign var val env'

bEval :: BExp -> Env -> Bool
bEval BTrue _ = True
bEval BFalse _ = False
bEval (aexp1 :==: aexp2) env = aEval aexp1 env == aEval aexp2 env
bEval (Not bexp) env = not (bEval bexp env)

aEval :: AExp -> Env -> Integer
aEval (Lit n) _ = n
aEval (aexp1 :+: aexp2) env = aEval aexp1 env + aEval aexp2 env
aEval (aexp1 :*: aexp2) env = aEval aexp1 env * aEval aexp2 env
aEval (Var name) env = mylookup env name
  where
    mylookup :: Env -> Name -> Integer
    mylookup [] _ = error "No such variable"
    mylookup ((var_name, var_val) : env') name
      | var_name == name = var_val
      | otherwise = mylookup env' name

factStmt :: Stmt
factStmt =
  "p" := Lit 1 ::: "n" := Lit 3
    ::: While
      (Not (Var "n" :==: Lit 0))
      ( "p" := Var "p" :*: Var "n"
          ::: "n" := Var "n" :+: Lit (-1)
      )

pg1 = Pgm [] factStmt