-- 1. Tipificat de números -------------------------------------------------------------
-- :t var -> retorna el TIPUS

p = 2
q = 1
-- stack ghci 02lab.hs
-- :t q -> Integer
-- :t p -> Integer

dr = p / q
-- :t q -> Double
-- :t p -> Double

de = div (round p) (round q) 
-- (/) :: Fractional a => a -> a -> a
-- div ->  Integers

--         3.8  -3.8
-- floor	  3	   -4
-- ceiling	4	   -3
-- truncate	3	   -3
-- round	  4	   -4

-- 2. Composició de funcions---------------------------------------------------------------
-- :i var , retorna  tipificacions de les funcions
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- ($) :: (a -> b) -> a -> b       -- Defined in ‘GHC.Internal.Base’

(.$.) :: (a -> b) -> a -> b -- aplicació
(.$.) = \f -> \x -> f x

(...) :: (b -> c) -> (a -> b) -> (a -> c) -- composició
(...) = \f -> \g -> \x -> f (g x)

arrodonir :: Double -> Integer -> Double
arrodonir valor numDec =
    let factor = fromInteger (10^numDec)
    in (/ factor) ... fromInteger ... round ... (* factor) .$. valor
{-
3.875
round 3.875 = 4
3.875 * 10^1 = 38.75
round 38.75 = 39
39 / 10^1 = 3.9
conclusió: numDec decimal -> multiplicar 10^numDec -> round -> dividir 10^numDec
-}
-- (/ factor) = \x -> x / factor

arrodonirb :: Double -> Integer -> Double
arrodonirb valor numDec =
    let factor = fromInteger (10 ^ numDec)
    in (/ factor) . fromInteger . round . (* factor) $ valor

-- infixr 9 .
-- infixr 0 $

quad :: Int -> Int
quad = \x -> x * x

multiplicar :: Int -> Int -> Int
multiplicar = \x -> \y -> x * y

triple :: Int -> Int
triple = \x -> x * 3

triplequad :: Int -> Int
triplequad = \x -> quad . triple $ x

multiplicarquad :: Int -> Int -> Int
multiplicarquad = \x -> \y -> quad . multiplicar x $ y

inc :: Int -> Int
inc = \x  -> x+1

-- f1 x = inc (inc (inc x) + (inc x))
f1 :: Int -> Int
f1 = \x -> inc $ inc (inc x) + inc x
-- f2 x = (+) (inc (inc x)) (inc (x + x))
f2 :: Int -> Int
f2 = \x -> (inc . inc $ x) + inc (x + x)

-- f(g(h(x))) => h x .> g .> f
(.>) :: a -> (a -> b) -> b
(.>) = \x -> \f -> f x

res = inc 0 .> inc .> inc

-- 3. Recursivitat -------------------------------------------------------------------
-- DIVISIO ENTERA
divisio :: Int -> Int -> Int
divisio x y
  | x < y = 0
  | otherwise = 1 + divisio (x - y) y
-- si x < y, no podem restar i es dona 0
-- si x >= y, restem una y del x i sumem 1

-- RESIDU
residu :: Int -> Int -> Int
residu x y
  | x < y = x
  | otherwise = residu (x - y) y
-- si x < y,  x es el residu
-- si x > y, restem y del x i seguim

-- COEFICIENT BINOMIAL
binom :: Int -> Int -> Int
binom n k
  | k == 0 = 1
  | n == 0 = 0
  | otherwise = binom (n-1) k + binom (n-1) (k-1)
-- n sobre 0 es 1
-- 0 sobre k es 0
-- (n sobre k) = (n-1 sobre k) + (n-1 sobre k-1)

-- SUMA DELS PRIMERS N NUMEROS
sumaN :: Int -> Int
sumaN n
  | n == 0 = 0
  | otherwise = n + sumaN (n - 1)
-- si arriba a ser  0 es retorna 0
-- en qualsevol altre cas es retorna la suma del matiex numero donat n, mes la suma dels primers n-1 nuemros...

-- SUMA DELS PRIMERS NUMEROS PARELLS
sumaNPar :: Int -> Int
sumaNPar n
  | n == 0 = 0
  | otherwise = 2*n + sumaNPar (n - 1)
-- si volem sumaNPar 4 -> 2+4+6+8 -> es suma dels 2*i
-- veiem que 8 coincideix amb 2*n, 6 a 2*(n-1)...

-- GENERALITZACIO DEL SUMATORI
sumaG :: (Int -> Int) -> Int -> Int
sumaG f n
  | n == 0 = 0
  | otherwise = f n + sumaG f (n - 1)
-- apliquem f a n en cada iteracio i pasem n-1 pel seguent sumant
-- f(n) + f(n-1) + ... + f(1)

-- GEN SUMA DELS PRIMERS N NUMEROS
fN :: Int -> Int
fN x = x
-- retorna el mateix nombre
sumaNG :: Int -> Int
sumaNG n = sumaG fN n

-- GEN SUMA DELS PRIMERS N NUMEROS PARELLS
fNPar :: Int -> Int
fNPar x = 2*x
-- retorna el doble del nombre
sumaNParG :: Int -> Int
sumaNParG n = sumaG fNPar n

-- SUMATORID
sumatoriD :: (Int -> Int) -> (Int -> Int) -> Int -> Int
sumatoriD f g n =
  sumaG fD n
  where
    fD i = sumaG gD n
      where
        gD j = f i * g j
-- es fa per cada f i la suma de j termes, cada terme correspon a un f i * g j

-- DERIVADA
drvd :: (Double -> Double) -> Double -> Double
drvd f p = iter 1
  where
  iter h
    |abs (d1 - d2) < 1e-6 = d2
    |otherwise = iter (h/2)
      where
        d1 = deriv h
        d2 = deriv (h/2)
        deriv h = (f (p + h) - f p) / h
-- en cada iteracio calcula la deriv aproximada amb el mateix h i el següent amb el h/2
-- si es menor que l'error 10^-6 
res01 = drvd (^2) (-2)
-- ghci> res01
-- -3.999999237130396