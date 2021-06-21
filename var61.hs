-- Micuda Andrei, Grupa 233
{-
Gasiti mai jos limbajul unui minicalculator si o interpretare partiala.
Calculatorul are doua celule de memorie. Interpretarea instructiunilor este data mai jos.

Un program este o expresie de tip `Prog`iar rezultatul executiei este starea finala a memoriei
Testare se face apeland `prog test`.
-}

type Env = (Int, Int) -- corespunzator celor doua celule de memorie

data Prog = On Env Stmt -- Env reprezinta valorile initiale ale celulelor de memorie

data Stmt
  = Off
  | Expr :<< Stmt -- evalueaza Expr, pune rezultatul in Mem1, apoi executa Stmt
  | Expr :< Stmt -- evalueaza Expr, pune rezultatul in Mem2, apoi executa Stmt

data Mem = Mem1 | Mem2

data Expr
  = M Mem
  | V Int
  | Expr :+ Expr
  | If1 Expr Expr
  | If2 Expr Expr

infixl 6 :+

infixr 2 :<

infixr 2 :<<

expr :: Expr -> Env -> Int
expr (e1 :+ e2) m = expr e1 m + expr e2 m
expr (M Mem1) m = fst m
expr (M Mem2) m = snd m
expr (V i) _ = i
expr (If1 e1 e2) (m1, m2) = case m1 of
  0 -> expr e2 (m1, m2)
  _ -> expr e1 (m1, m2)
expr (If2 e1 e2) (m1, m2) = case m2 of
  0 -> expr e2 (m1, m2)
  _ -> expr e1 (m1, m2)

stmt :: Stmt -> Env -> Env
stmt Off m = m
stmt (ex :<< st) (m1, m2) = let val = expr ex (m1, m2) in stmt st (val, m2)
stmt (ex :< st) (m1, m2) = let val = expr ex (m1, m2) in stmt st (m1, val)

prog :: Prog -> Env
prog (On m s) = stmt s m

test1 = On (1, 2) (V 3 :< M Mem1 :+ V 5 :<< Off)

test2 = On (0, 0) (V 3 :<< M Mem2 :+ V 5 :< Off)

test3 = On (0, 1) (V 3 :<< V 4 :< M Mem1 :+ M Mem2 :+ (V 5) :< Off)

test4 = On (-2, 3) (M Mem1 :+ V 3 :< Off)

test5 = On (0, 5) ((If1 (M Mem1 :+ V 5) (M Mem2 :+ V 5)) :+ V 21 :<< Off) -- (31, 5)

test6 = On (7, 2) ((If2 (M Mem2 :+ V 5) (M Mem1 :+ V 5)) :< Off) -- (7, 7)

test7 = On (-5, 5) ((If1 (V 17) (M Mem1 :+ V (-10))) :< Off) -- (-5, 17)

test8 = On (7, 0) ((If2 (M Mem2 :+ V 5) (M Mem1 :+ V 5)) :+ V (-24) :< Off) -- (7, -12)

-- CERINTA 3
type Env3 = ((Int, Int), Int) -- corespunzator celor doua celule de memorie si numarului total de accesari ale memoriei

data Prog3 = On3 Env Stmt -- Env reprezinta valorile initiale ale celulelor de memorie

-- evaluarea unei expresii va intoarce valoarea expresiei si numarul de accesari modificat
expr3 :: Expr -> Env3 -> (Int, Int)
-- expr3 (e1 :+ e2) m = expr3 e1 m + expr3 e2 m
expr3 (e1 :+ e2) (m, acc) =
  let (v1, acc1) = expr3 e1 (m, acc)
   in let (v2, acc2) = expr3 e2 (m, acc1)
       in (v1 + v2, acc2)
expr3 (M Mem1) (m, acc) = (fst m, acc + 1)
expr3 (M Mem2) (m, acc) = (snd m, acc + 1)
expr3 (V i) (m, acc) = (i, acc)
expr3 (If1 e1 e2) ((m1, m2), acc) = case m1 of
  0 -> expr3 e2 ((m1, m2), acc)
  _ -> expr3 e1 ((m1, m2), acc)
expr3 (If2 e1 e2) ((m1, m2), acc) = case m2 of
  0 -> expr3 e2 ((m1, m2), acc)
  _ -> expr3 e1 ((m1, m2), acc)

stmt3 :: Stmt -> Env3 -> Env3
stmt3 Off m = m
stmt3 (ex :<< st) ((m1, m2), acc) = let (val, acc') = expr3 ex ((m1, m2), acc) in stmt3 st ((val, m2), acc' + 1)
stmt3 (ex :< st) ((m1, m2), acc) = let (val, acc') = expr3 ex ((m1, m2), acc) in stmt3 st ((m1, val), acc' + 1)

prog3 :: Prog3 -> Env3
prog3 (On3 m s) = stmt3 s (m, 0)

test3_1 = On3 (1, 2) (V 3 :< M Mem1 :+ V 5 :<< Off) -- ((6, 3), 3)

test3_2 = On3 (0, 0) (V 3 :<< M Mem2 :+ V 5 :< Off) -- ((3, 5), 3)

test3_3 = On3 (0, 1) (V 3 :<< V 4 :< M Mem1 :+ M Mem2 :+ (V 5) :< Off) -- ((3, 12), 5)

test3_4 = On3 (-2, 3) (M Mem1 :+ V 3 :< Off) -- ((-2, 1), 2)

test3_5 = On3 (0, 5) ((If1 (M Mem1 :+ V 5) (M Mem2 :+ V 5)) :<< Off) -- ((10, 5), 2)

test3_6 = On3 (7, 2) ((If2 (M Mem2 :+ V 5) (M Mem1 :+ V 5)) :< Off) -- ((7, 7), 2)

{-CERINTE

1) (10pct) Finalizati definitia functiilor de interpretare.
2) (10 pct) Adaugati expresiile `If1 e1 e2` si `If2 e1 e2`  care evaluează  `e1` daca `Mem1`,
respectiv `Mem2`, este nenula si`e2` in caz contrar.
3) (20pct) Definiti interpretarea  limbajului extins  astfel incat executia unui program  sa calculeze memoria finala,
  si numărul de accesări (scrieri și citiri) ale memoriilor `Mem1` si `Mem2` (se va calcula o singura
  valoare, insumand accesarile ambelor memorii, fara a lua in considerare initializarea).
  Rezolvați subiectul 3) în același fișier, redenumind funcțiile de interpretare.

Indicati testele pe care le-ati folosit in verificarea solutiilor.

-}