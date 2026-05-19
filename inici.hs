-- 1. Entorn de treball ------------- stack ghci inici.hs
-- Exemple
inc :: Double -> Double
inc i = i + 1

-- 2. Funcions
-- Exercici
primer :: a -> b -> a
primer x y = x

segon  :: a -> b -> b
segon x y = y
-----------------------------------------------------------------------------
-- 2.1 Notacio lambda
-- Exercici
primerL :: a -> b -> a
primerL = \x -> (\y -> x)

segonL :: a -> b -> b
segonL = \x -> (\y -> y)

-- 2.2 Tipicitat
-- Exercici: Tipificar totes les funcions que s’han definit fins ara

--2.3 Prioritat/Associativitat
-- Exercici
{-
• r1 = primer segon 1 2 3 -> r1 = 2
• r2 = segon primer 1 2 3 -> r2 = 3
• r3 = primer 1 + segon 1 2 + 1 2 -> r3 = 4
• r4 = primer segon 1 2 3 -> r4 = 3
-}

r1 = primer (segon 1 2) 3
r2 = segon (primer 1 2) 3 
r3 = primer (1 + (segon 1 2) + 1) 2
r4 = (primer segon 1) 2 3

-- 2.4 Notació infixa
{- Exercici: Definir les funcions (?) i (??) equivalents a primer i segon respectivament. Reescriure les
expressions de l’exercici anterior en notació infixa (anomenar-les r1i ...) -}

(?) :: a -> b -> a
-- (?) = primer
-- (?) = \x -> (\y -> x)
(?) x y = x

(??) :: a -> b -> b
-- (??) = segon
-- (??) = \x -> (\y -> y)
(??) x y = y

r1i = (1 ?? 2) ? 3
r2i = (1 ? 2) ?? 3
r3i = (1 + (1 ?? 2) + 1) ? 2
r4i = ((?) (??) 1) 2 3

-- 2.5 Funcions d’ordre superior
{- Exercici 1: Definir la funció inter que intercanvia l’ordre dels paràmetres d’una altra funció de dos
paràmetres. Per exemple inter primer 1 2 retorna 2 -}
inter :: (a -> b -> a) -> (b -> a -> a)
inter f x y = f y x
-- comprueba inter primer 1 2 -> 2

-- Exercici 2: Definir a partir de la funció primer la funció segonI equivalent a la funció segon
segonI :: a -> b -> b
segonI = inter primer
-- comprueba segonI 1 2 -> 2

-- Exercici 3: Parentitzar les següents expressions de manera que sempre retornin 0
{-
• e1 = (+) inter (-) 1 2 -1
• e2 = primer primer primer 0 0 0 0
• e3 = 0 + primer segon 1 0 1
• e4 = div e1 e3, es poden reparentitzar e1 i/o e3 
-}

e1 = ((+) (inter (-) 1 2)) (-1)
e2 = primer ((primer (primer 0)) 0 0) 0
e3 = 0 + (primer (segon 1 0) 1)
e3_4 = 0 + (primer segon 1) 0 1
e4 = div e1 e3_4

-- 2.6 Aplicació parcial
{- Exercici: Definir una funció que donats un pendent i un valor de tall amb l’eix d’ordenades retorna una
funció que denoti l’equació de la recta. Definir una funció que retorni una funció que denoti l’equació de
totes les possibles rectes de pendent 4. -}
denotiEquacio :: Double -> Double -> (Double -> Double)
denotiEquacio m n = \x -> m * x + n

denotiEquacio4 :: Double -> (Double -> Double)
denotiEquacio4 n = denotiEquacio 4 n
--------------------------------------------------------------------------------------------------------------------
-- 3 Tipus
-- Exercici 1: Exercici: Definir el tipus M3 que conté tres valors 𝙼𝟹 = {𝙰, 𝙱, 𝙲}.
data M3 = A | B | C
-- Exercici 2: Sigui la funció
cert :: a -> b -> a
cert x y = x
{- Definir la funció fals i l’operador (funció) d’ordre (.<.) tal que representi l’ordre A < B < C, és a dir,
per exemple A .<. B = cert i C .<. B = fals. Tipificar la funció (.<.) -}
fals :: a -> b -> b 
fals x y = y

(.<.) :: M3 -> M3 -> (a -> a -> a)
(.<.) A B = cert
(.<.) B C = cert
(.<.) A C = cert
(.<.) _ _ = fals

{-
Exercici: Definir la funció maxim :: M3 -> M3 -> M3 -> M3, que donats tres valors de tipus M3
retorna el màxim. Si en la definició que has fet es calcula un mateix valor dues vegades redefinir la funció
de dues maneres:
• Fent servir una expressió let ... in.
• Fent servir un bloc where.
-}
max2 :: M3 -> M3 -> M3
max2 x y = (x .<. y) y x
-- cert retorna el primer param. (y)
-- fals retorna el segon param. (x)

maximLetin :: M3 -> M3 -> M3 -> M3
maximLetin x y z =
  let m = max2 x y
  in max2 m z

maximWhere :: M3 -> M3 -> M3 -> M3
maximWhere x y z = max2 m z
  where
    m = max2 x y

mostrarM3 :: M3 -> Char
mostrarM3 A = 'A'
mostrarM3 B = 'B'
mostrarM3 C = 'C'

-- exemple d'execució: mostrarM3 (maximWhere  A B B)