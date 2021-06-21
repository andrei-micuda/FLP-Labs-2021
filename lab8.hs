import Data.List

data Variable = Variable String Int

var :: String -> Variable
var x = Variable x 0

instance Show Variable where
  show (Variable x 0) = x
  show (Variable x i) = x ++ "_" ++ show i

instance Eq Variable where
  (Variable v1 i1) == (Variable v2 i2) = (v1 == v2) && (i1 == i2)

fresh :: Variable -> [Variable] -> Variable
fresh (Variable str int) lst_var = (Variable str (int_new + 1)) where int_new = maximum $ map (\(Variable _ i) -> i) ((filter (\(Variable str' _) -> str' == str) ((Variable str int) : lst_var)))

data Term
  = V Variable
  | App Term Term
  | Lam Variable Term

v :: String -> Term
v x = V (var x)

lam :: String -> Term -> Term
lam x = Lam (var x)

lams :: [String] -> Term -> Term
lams xs t = foldr lam t xs

($$) :: Term -> Term -> Term
($$) = App

infixl 9 $$

insertParens :: String -> String
insertParens str = "(" ++ str ++ ")"

instance Show Term where
  show (V var) = show var
  show (App term1 term2) = insertParens $ show term1 ++ " " ++ show term2
  show (Lam var term) = insertParens $ "\\" ++ show var ++ "." ++ show term

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x : xs) = x : (removeDuplicates (filter (/= x) xs))

freeVars :: Term -> [Variable]
freeVars (V var) = [var]
freeVars (App term1 term2) = removeDuplicates $ freeVars term1 ++ freeVars term2
freeVars (Lam var term) = removeDuplicates $ filter (/= var) $ freeVars term

allVars :: Term -> [Variable]
allVars (V var) = [var]
allVars (App term1 term2) = removeDuplicates $ allVars term1 ++ allVars term2
allVars (Lam var term) = removeDuplicates $ var : allVars term

subst :: Term -> Variable -> Term -> Term -- [u/x]t
subst u x (V var)
  | x == var = u
  | otherwise = V var
subst u x (App term1 term2) = (App (subst u x term1) (subst u x term2))
subst u x (Lam y t)
  | x == y = Lam y t -- x este legata, nu fac subst
  | y `notElem` freeVarsU = Lam y (subst u x t)
  | x `notElem` freeVarsT = Lam y t
  | otherwise = Lam y' (subst u x t')
  where
    freeVarsT = freeVars t
    freeVarsU = freeVars u
    allFreeVars = nub ([x] ++ freeVarsU ++ freeVarsT)
    y' = fresh y allFreeVars
    t' = (subst (V y') y t)

aEq :: Term -> Term -> Bool
aEq (V var1) (V var2) = var1 == var2
aEq (App term11 term12) (App term21 term22) = aEq term11 term21 && aEq term12 term22
aEq (Lam vr term) (Lam vr' term') = (vr == vr' && aEq term term') || (aEq (subst (v "y") vr term) (subst (v "y") vr' term'))
aEq _ _ = False

t1 = aEq (lam "x" (v "x")) (lam "y" (v "y"))

t2 = aEq (lam "x" (v "x")) (lam "y" (v "z"))

oneStepReduce :: Term -> Maybe Term
oneStepReduce (App (Lam x t) u) = Just $ subst u x t
oneStepReduce (App t1 t2) =
  case t1Red of
    Just t1' -> Just (App t1' t2)
    Nothing -> case t2Red of
      Just t2' -> Just (App t1 t2')
      Nothing -> Nothing
  where
    t1Red = oneStepReduce t1
    t2Red = oneStepReduce t2
oneStepReduce (Lam x t) =
  case tRed of
    Just t' -> Just $ Lam x t'
    Nothing -> Nothing
  where
    tRed = oneStepReduce t
oneStepReduce _ = Nothing

reduce :: Term -> Term
reduce t =
  case tRed of
    Just t' -> reduce t'
    Nothing -> t
  where
    tRed = oneStepReduce t