-- 1.1 Expressions de tipus
-- BIT
data Bit = O | I
-- COMPARAR BITS
(.==.) :: Bit -> Bit -> Bool
(.==.) O O = True
(.==.) I I = True
(.==.) _ _ = False

-- 1.2 Constructors de valors paramètrics
--  DBIT
data DBit = D Bit Bit
bitAint :: Bit -> Int
bitAint O = 0
bitAint I = 1
-- per pasar cada bit particular a int
dbitAint :: DBit -> Int
dbitAint (D x y) = bitAint y + 2* bitAint x

-- 1.3 Patrons
data G = G1 Int Int
divG (G1 x y) = if y /= 0 then div x y else undefined

dbitAintP :: DBit -> Int 
dbitAintP (D O O) = 0
dbitAintP (D O I) = 1
dbitAintP (D I O) = 2
dbitAintP (D I I) = 3

-- 1.4 Definicions recursives
data Nat = Zero | S Nat
natAint :: Nat -> Int
natAint Zero = 0
natAint (S x) = 1 + natAint(x)
-- si es zero = 0
-- si es S Nat llavors comptem 1 i seguim amb el Nat

sumaNat' :: Nat -> Nat -> Nat
sumaNat' x y = go n
  where 
    n = natAint x + natAint y

    go 0 = Zero
    go k = S $ go (k-1)
-- cada S es com +1
-- trobar el int que correspon i tornar a construir un Nat
sumaNat :: Nat -> Nat -> Nat
sumaNat Zero n = n
sumaNat (S m) n = S (sumaNat m n)
-- sense usar la Int dins d'ell, directament am Nat
-- si es Zero y un Nat seria el mateix Nat
-- si es Nat i un Nat llavors es treu de un en un del primer Nat
-- si (S m) treiem la S fora [com si treiem 1 del primer nombre natural] 
-- i continuem sumant la m, que potser un Nat o Zero, amb la n

data MInt = Pos Nat | Neg Nat

mintAint :: MInt -> Int
mintAint (Pos x) = natAint x
mintAint (Neg x) = -(natAint x)

intAmint :: Int -> MInt
intAmint n
  |n >= 0 = Pos $ go n
  |otherwise = Neg $ go (-n)
    where 
      go 0 = Zero
      go k = S $ go (k-1)
-- utilitzem Nat com el valor absolut del MInt per tant nomes cal afegir signe

sumaMInt :: MInt -> MInt -> MInt
sumaMInt x y = intAmint n
  where 
    n = mintAint x + mintAint y

resta :: Nat -> Nat -> MInt
resta Zero Zero = Pos Zero
resta (S x) Zero = Pos (S x)
resta Zero (S y) = Neg (S y)
resta (S x) (S y) = resta x y

sumaMIntD :: MInt -> MInt -> MInt
sumaMIntD (Pos x) (Pos y) = Pos (sumaNat x y)
sumaMIntD (Neg x) (Neg y) = Neg (sumaNat x y)
sumaMIntD (Pos x) (Neg y) = resta x y
sumaMIntD (Neg x) (Pos y) = resta y x

-- 1.5 Constructors de tipus paramètrics

-- 1.6 Llistes
data Llista a = B | L a (Llista a) deriving Show -- Permet mostrar Llistas per pantalla


-- Coonvertir de Llista a una llista normal de haskell
desdeL :: Llista a -> [a]
desdeL B = []
desdeL (L x xs) = x : desdeL xs
{-
desdeL (L 1 (L 2 (L 3 B)))

= 1 : desdeL (L 2 (L 3 B))
= 1 : (2 : desdeL (L 3 B))
= 1 : (2 : (3 : desdeL B))
= 1 : (2 : (3 : []))
= [1,2,3]
-}

-- Convertir d'una llista normal de haskell a Llista
aL :: [a] -> Llista a
aL [] = B
aL (x:xs) = L x (aL xs)
-- [] y B son equivalentes -> vacio
-- L x xs equivale a x:xs

-- aL . desdeL || Llista -> [a] -> Llista
-- desdeL . aL || [a] -> Lista -> [a]

-- 1. Crea una llista amb els n enters (de n a 1)
initL :: Int -> Llista Int
initL 0 = B
initL n = L n (initL (n-1))

-- 2. Gira el contingut d’una llista
-- auxiliar per afegir un element al final d'una llista
afegirFinal :: a -> Llista a -> Llista a
afegirFinal x B = L x B
afegirFinal x (L y ys) = L y (afegirFinal x ys)
-- Girar la llista
giraL :: Llista a -> Llista a
giraL B = B
giraL (L x xs) = afegirFinal x (giraL xs)
{-
= afegirFinal 1 (giraL (L 2 (L 3 B)))
= afegirFinal 1 (afegirFinal 2 (giraL (L 3 B)))
= afegirFinal 1 (afegirFinal 2 (afegirFinal 3 (giraL B)))
= afegirFinal 1 (afegirFinal 2 (afegirFinal 3 B))
= afegirFinal 1 (afegirFinal 2 (L 3 B))
= afegirFinal 1 (L 3 (L 2 B))
= L 3 (L 2 (L 1 B))
-}

-- comprovacio
llistagirada = desdeL $ giraL $ aL [1,2,3]
llistagirada2 = desdeL $ giraL $ initL 5

-- 3. llista de llistes a partir d’un enter
initLdL :: Int -> Llista (Llista Int)
initLdL 0 = B
initLdL n = L (initL n) (initLdL (n-1))

desdeLdL :: Llista (Llista a) -> [[a]]
desdeLdL B = []
desdeLdL (L xs xss) = desdeL xs : desdeLdL xss
-- observem que (desdeL xs) es [a] i (desdeLdL xss) retornarà [[a]] 

-- 4. Aplasta una llista de llistes en una llista d’elements
-- concatenar dos llistes
concatL :: Llista a -> Llista a -> Llista a
concatL xs' B = xs'
concatL B ys = ys
concatL (L x xs) ys = L x (concatL xs ys)
-- si una està buida es retorna l'altre
-- si la primera es L x xs, conservem x y seguim concatenant el que queda de las xs

-- aplastar la llista
aplastaL :: Llista (Llista a) -> Llista a
aplastaL B = B
aplastaL (L xs xss) = concatL xs (aplastaL xss)
-- si es L xs xss llavors concatenem xs amb els següents llistes dins la llista i seguim

-- Defineix la funció de mapeig mapejaL
mapejaL :: (a -> b) -> Llista a -> Llista b
mapejaL f B = B
mapejaL f (L x xs) = L (f x) (mapejaL f xs)
-- comprovacio
m_ = mapejaL even (L 1 (L 2 (L 3 B)))

-- Defineix la funció initLdL2 no recursiva, equivalent a initLdL, usant mapejaL i initL.
initLdL2 :: Int -> Llista (Llista Int)
initLdL2 n = mapejaL initL (initL n)

-- Defineix la funció desdeLdL2 no recursiva, equivalent a desdeLdL, usant mapejaL i desdeL.
desdeLdL2 :: Llista (Llista a) -> [[a]]
desdeLdL2 xs = desdeL $ mapejaL desdeL xs
-- xs :: Llista (Llista a)
-- mapejaL vol Llista a
-- mapejaL desdeL xs :: Llista [a]
-- desdeL converteix (Llista a) a [a]

-- comprovacio
l_ = initLdL 3
l_' = initLdL2 3

d_ = desdeLdL l_
d_' = desdeLdL2 l_'

