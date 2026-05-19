import Control.Monad.State

import Data.Hashable
-- stack install hashable 
import Data.Int
import Data.Char


-- 'e' és el tipus de l'estat
-- 'a' és el tipus del resultat
data Estat e a = Estat (e -> (a, e))


-- extreu la funció
execEstat :: Estat e a -> e -> (a, e) -- Estat e a -> (e -> (a, e))
execEstat (Estat g) = g

-- retorna la funcio que aplicada un estat
-- retona el valor resultat
execValor :: Estat e a -> e -> a
execValor (Estat g) = fst . g


-- obtenir l'estat
obtenir :: Estat e e
obtenir = Estat $ \est -> (est, est)


-- canviar l'estat
canviar :: e -> Estat e ()
canviar est = Estat $ \_ -> ((), est)


---------------------------------------------------------------------
-----------------------      Exemple 1       ------------------------
---------------------------------------------------------------------

-- insereix un enter a la pila
posarBind :: Integer -> Estat [Integer] ()
-- posar i =  Estat $ \pila -> ((), i:pila)
-- aixo dona una accio que s'haura d'executar mes endavant
posarBind i =  obtenir >>= (\pila -> canviar (i:pila))

-- insereix un enter a la pila
posar :: Integer -> Estat [Integer] ()
posar i =  do
    pila <- obtenir
    canviar (i:pila)

-- Treu el primer element de la pila, i retorna l'element tret
-- Precondicio: Llista no buida
treure :: Estat [Integer] Integer
treure = do
    pila <- obtenir
    canviar (tail pila)
    pure (head pila)

-- Treu el primer element de la pila
-- Precondicio: pila no buida
treureBind :: Estat [Integer] Integer
treureBind =  undefined

-- Consulta el primer element de la pila (sense treure'l)
-- Precondicio: pila no buida
ultim :: Estat [Integer] Integer
ultim =  do
    pila <- obtenir
    pure (head pila)

-- Consulta el primer element de la pila (sense treure'l)
-- Precondicio: pila no buida
ultimBind :: Estat [Integer] Integer
ultimBind =  undefined


-- Treu els 2 primers enters de la pila 
-- fa la suma i la torna a posar a la pila
sumar :: Estat [Integer] ()
sumar =  do
    pila <- obtenir
    x <- treure
    y <- treure
    posar (x + y)


-- Treu els 2 primers enters de la pila 
-- fa la suma i la torna a posar a la pila
sumarBind :: Estat [Integer] ()
sumarBind =  undefined


-- Treu els 2 primers enters de la pila 
-- els multiplica i posa el resultat a la pila
mult :: Estat [Integer] ()
mult =  do
    pila <- obtenir
    x <- treure
    y <- treure
    posar (x * y)

-- Treu els 2 primers enters de la pila 
-- els multiplica i posa el resultat a la pila
multBind :: Estat [Integer] ()
multBind =  undefined

-- Canvia el signe del primer element de la pila
oposat :: Estat [Integer] ()
oposat =  do
    pila <- obtenir
    x <- treure
    posar (-x)

-- Canvia el signe del primer element de la pila
oposatBind :: Estat [Integer] ()
oposatBind = treure >>= (\x -> posar ((-1)*x) )


-- posar un enter i obtenir la pila amb l'enter 
ex1 :: ([Integer], [Integer])
-- hem de construir una capsa que si li passo [] buida retorni [4], passo [6, 5] retorni [4, 6, 5]
ex1 = execEstat (do
    posar 4
    obtenir
    )
    [6, 5]

-- posar un enter i obtenir la pila amb l'enter 
ex1Bind =  undefined


-- canviar la pila perque nomes contingui els valors 1 i 2
-- despres afegir l'enter 3 i retornar l'enter 10.
ex2 :: (Integer, [Integer])
ex2 =  execEstat (do
    canviar [1, 2]
    posar 3
    pure 10
    )
    [3, 4]

-- (a+b)*c
ex3 :: Estat [Integer] Integer
-- es una estructura
-- [a, b, c] -> a+? | [b, c] -> a+b | [c] -> [a+b, c] -> (a+b) | [c] -> (a+b)*c [] -> [(a+b)*c]
ex3 =  do
    x <- sumar
    mult
    ultim


-- (2+3)*6
execEx3 :: Integer
execEx3 =  execValor ex3 [2,3,4,5,6]

execEstatEx3 = execEstat ex3 [2,3,4,5,6]


---------------------------------------------------------------------
-----------------------      Exemple 2       ------------------------
---------------------------------------------------------------------

-- A partir d'una llavor (de tipus String) que es l'estat inicial, 
-- produeix un enter pseudoaleatori (hash de la llavor) i canvia la 
-- llavor a una nova que és la concatenació de la llavor amb si 
-- mateixa

enterPsd :: Estat String Int
enterPsd =  undefined


tupla4EntersPsd :: Estat String (Int,Int,Int,Int)
tupla4EntersPsd = undefined

-- quin valor retorna?
tupla4ValorsPsd = execValor tupla4EntersPsd "b"


llistaEntersPsd :: Estat String [Int]
llistaEntersPsd =  undefined

exEntersPsd = execEstat llistaEntersPsd "abcd"



---------------------------------------------------------------------
----------------------      Instancies       ------------------------
---------------------------------------------------------------------

instance Functor (Estat e) where
    -- fmap :: (a -> b) -> Estat e a -> Estat e b
    fmap f (Estat g) = Estat $ \est -> 
        let (v, est') = g est 
        in (f v, est')

instance Applicative (Estat e) where
    -- pure :: a -> Estat e a
    pure x = Estat $ \est -> (x, est)
    -- (<*>) :: Estat e (a -> b) -> Estat e a -> Estat e b
    -- est ->| ->| fg |-> (g, est1) <*> est1 ->| fv |-> (v2, est2) ->|-> (g v2, est2)
    -- fg = Estat e (a->b)
    -- fv = Estat e a
    Estat fg <*> Estat fv = Estat $ \est -> 
        let (g, est1)  = fg est
            (v2, est2) = fv est1
        in (g v2, est2)


instance Monad (Estat e) where
    return = pure
    (>>=) :: Estat e a -> (a -> Estat e b) -> Estat e b
    -- est ->| ->|fx| -> (x, est1) -> est1 -> |h| -> | -> (y, est2) 
    (Estat fx) >>= g = Estat $ \est -> 
        let (x, est1) = fx est
            Estat h = g x
        in h est1