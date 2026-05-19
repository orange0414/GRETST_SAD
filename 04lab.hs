-- en 3.1 utilitzar foldable --

------------------------------ 1 Classes i instàncies ------------------------------
class Iterador t where
    ele::t a -> a
    seg::t a -> t a
    hasnext::t a -> Bool

-- Practica anterior --------------------------------
data Llista a = Buida | L a (Llista a) deriving Show
data Nat = Zero | S Nat deriving Show
natAint :: Nat -> Int
natAint Zero = 0
natAint (S x) = 1 + natAint(x)
-----------------------------------------------------
instance Iterador Llista where
    ele (L x _) = x 
    seg (L _ xs) = xs
    hasnext Buida = False
    hasnext (L x _) = True

-- ghci> :i Eq
-- type Eq :: * -> Constraint
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
-- {-# MINIMAL (==) | (/=) #-}
-- com a minim hem de tenir definit un dels dos
instance Eq Nat where
    Zero == Zero = True
    _ == Zero = False
    Zero == _ = False
    (S n) == (S m) = (n == m)

-- ghci> :i Ord
-- type Ord :: * -> Constraint
-- class Eq a => Ord a where
--   compare :: a -> a -> Ordering
--   (<) :: a -> a -> Bool
--   (<=) :: a -> a -> Bool
--   (>) :: a -> a -> Bool
--   (>=) :: a -> a -> Bool
--   max :: a -> a -> a
--   min :: a -> a -> a
-- {-# MINIMAL compare | (<=) #-}
-- com a minim s'ha de definir (<=) o compare
instance Ord Nat where
    Zero <= _ = True
    _ <= Zero = False
    (S n) <= (S m) = (n <= m)

-- ghci> :i Enum
-- type Enum :: * -> Constraint
-- class Enum a where
--   succ :: a -> a
--   pred :: a -> a
--   toEnum :: Int -> a
--   fromEnum :: a -> Int
--   enumFrom :: a -> [a]
--   enumFromThen :: a -> a -> [a]
--   enumFromTo :: a -> a -> [a]
--   enumFromThenTo :: a -> a -> a -> [a]
--   {-# MINIMAL toEnum, fromEnum #-}
-- com a minim hem de tenir definit toEnum, fromEnum
instance Enum Nat where
    toEnum 0 = Zero
    toEnum n = S (toEnum (n-1))

    fromEnum = natAint

------------------------------ 2 Les classes Semigroup i Monoid ------------------------------
{-
ghci> :i Semigroup
type Semigroup :: * -> Constraint
class Semigroup a where
  (<>) :: a -> a -> a
  GHC.Internal.Base.sconcat :: GHC.Internal.Base.NonEmpty a -> a
  GHC.Internal.Base.stimes :: Integral b => b -> a -> a
  {-# MINIMAL (<>) | sconcat #-}
-}

{-
1) x <> y = y ++ x
2) x <> y = x ++ y
3) x <> y = x : y
-}

-- les correctes son 1) i 2) perque (x ++ y) ++ z = x ++ (y ++ z) compleix l'associativitat, igual canviant ordre de x y
{-
ghci> :t (++)
(++) :: [a] -> [a] -> [a]
-}

-- 3) es incorrecte, x : (y : zs) si que funcionaria be, pero (x : y) : zs no ja que espera una 'a' en lloc de '[a]' -> x:y es [a]
{-
ghci> :t (:)
(:) :: a -> [a] -> [a]
-}

-- Practica anterior ------------------
sumaNat :: Nat -> Nat -> Nat
sumaNat Zero n = n
sumaNat (S m) n = S (sumaNat m n)
---------------------------------------

instance Semigroup Nat where
    (<>) = sumaNat

instance Monoid Nat where
    mempty = Zero
-- S(S Zero) <> mempty = S(S Zero)
-- mempty <> S(S Zero) = S(S Zero)

------------------------------ 3 La classe Foldable ------------------------------
instance Foldable Llista where
    foldr f v Buida = v
    foldr f v (L x xs) = f x (foldr f v xs)

-- No es pot definir Foldable per a Nat perquè Nat no és un tipus contenidor.
-- Foldable requereix un tipus de forma * -> *, i Nat té forma *.

-- ghci> :i Foldable
-- type Foldable :: (* -> *) -> Constraint

-- ghci> :t foldr
-- foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

-- ghci> :i Nat
-- type Nat :: *

foldNat :: (a -> a) -> a -> Nat -> a
foldNat f x Zero  = x
foldNat f x (S n) = f (foldNat f x n)
-- amb Zero, no aplica cap vegada la funció
-- amb S n, aplica una vegada més la funció

natAInt :: Nat -> Int
natAInt = foldNat ((+) 1) 0
-- per cada constructor S sumem 1
-- començar a comptar des de 0, element neutre

sumNat :: Nat -> Nat -> Nat
sumNat n m = foldNat S m n
-- per cada constructor S de n que tingui, paliquem una S a m

------------------------------ 3.1 Exercicis amb llistes ------------------------------
{-
ghci> :i Foldable
type Foldable :: (* -> *) -> Constraint
class Foldable t where
  GHC.Internal.Data.Foldable.fold :: Monoid m => t m -> m
  foldMap :: Monoid m => (a -> m) -> t a -> m
  GHC.Internal.Data.Foldable.foldMap' :: Monoid m =>
                                         (a -> m) -> t a -> m
  foldr :: (a -> b -> b) -> b -> t a -> b
  GHC.Internal.Data.Foldable.foldr' :: (a -> b -> b) -> b -> t a -> b
  foldl :: (b -> a -> b) -> b -> t a -> b
  foldl' :: (b -> a -> b) -> b -> t a -> b
  foldr1 :: (a -> a -> a) -> t a -> a
  foldl1 :: (a -> a -> a) -> t a -> a
  GHC.Internal.Data.Foldable.toList :: t a -> [a]
  null :: t a -> Bool
  length :: t a -> Int
  elem :: Eq a => a -> t a -> Bool
  maximum :: Ord a => t a -> a
  minimum :: Ord a => t a -> a
  sum :: Num a => t a -> a
  product :: Num a => t a -> a
-}

maxInt :: [Int] -> Int
maxInt = maximum

numNothings :: [Maybe a] -> Int
numNothings = foldr f 0
  where
    f Nothing acc = 1 + acc
    f (Just _) acc = acc

maxMin :: [Int] -> (Int,Int)
maxMin [] = (0,0)
maxMin (x:xs) = foldr f (x,x) xs
  where
    f y (maxim,minim) = (max y maxim, min y minim)
-- y representa uno de esos enteros

numISuma :: [Int] -> (Int, Int)
numISuma = foldr f (0,0)
  where
    f x (n,s) = (n+1, s+x)

mitjana :: [Int] -> Float
mitjana [] = 0
mitjana xs = fromIntegral s / fromIntegral n
  where
    (n,s) = numISuma xs

totsMax :: [Int] -> [Int]
totsMax [] = [0]
totsMax xs = foldr f [] xs
  where
    m = maxInt xs
    f x acc
      | x == m = x : acc
      | otherwise = acc
-- m = maximum xs calcula el valor maxim
-- si un element x es igual a m, l'afegim

posicionsLletra :: Char -> String -> [Int]
posicionsLletra c xs = foldr f [] (zip [1..] xs)
  where
    f (n,x) acc
      | x == c = n : acc
      | otherwise = acc
-- construim parells de posicio y la lletra corresponent
-- si la lletra x coincideix amb el que busquem c, afegim la seva parella n en la llista

sumPar :: [[Int]] -> [Int]
sumPar = foldr f []
  where
    f xs acc = foldr (+) 0 xs : acc
-- amb foldr fem suma de 0(neutre) amb tots els elements de cada subllista

sumTot :: [[Int]] -> Int
sumTot = foldr f 0
  where
    f xs acc = foldr (+) 0 xs + acc

separar :: Ord a => [a] -> a -> ([a],[a])
separar xs v = foldr f ([],[]) xs
  where
    f x (menors,majors)
      | x <= v    = (x:menors, majors)
      | otherwise = (menors, x:majors)

ordenar :: Ord a => [a] -> [a]
ordenar []     = []
ordenar (x:xs) = ordenar menors ++ [x] ++ ordenar majors
  where
    (menors, majors) = separar xs x