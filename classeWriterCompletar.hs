{-
    - data Maybe a = Just a | Nothing
    - data Reader r a = R (r -> a)
    - data State e a = S (s -> (a, s))
-}

--data Writer w a = W (a, w)
newtype Writer w a = Writer { runWriter :: (a, w) } deriving Show
-- a: tipues de valor inicial
-- w: tipus de registre

instance Functor (Writer w) where
    -- fmap :: (a -> b) -> Writer w a -> Writer w b
    fmap f (Writer (x, registre)) = (Writer (f x, registre))

-- Monoid w
instance Monoid w => Applicative (Writer w) where
    -- pure :: a -> Writer w a
    -- Crea un Writer amb el valor x i un registre buit.
    -- mempty és el valor neutre del Monoid:
    --   []   per a llistes
    --   ""   per a String
    --   Sum 0 per a sumes
    pure x = Writer (x, mempty)

    -- (<*>) :: Writer w (a -> b) -> Writer w a -> Writer w b
    -- Aplica la funció f al valor x i combina els dos registres.
    -- (<>) és l'operació de combinació del Monoid:
    --   [1,2] <> [3,4] = [1,2,3,4]
    --   "abc" <> "def" = "abcdef"
    --   Sum 2 <> Sum 3 = Sum 5
    Writer (f, registre1) <*> Writer (x, registre2) = Writer (f x, registre1 <> registre2)


instance Monoid w => Monad (Writer w) where
    return = pure
    -- (>>=) :: Writer w a -> (a -> Writer w b) -> Writer w b
    -- (>>=) extreu el valor x, aplica f x i concatena els registres.
    Writer (x, registre1) >>= f =
        let Writer (y, registre2) = f x
        in Writer (y, registre1 <> registre2)

tell :: Monoid w => w -> Writer w ()
tell registrarMiss = Writer ((), registrarMiss)

{---------------------------------------------------------------------------------



Calculadora que utilitza el monad Writer per acumular un registre d'operacions 
juntament amb els resultats matemàtics.

Construir una calculadora que no només retorni el resultat final, 
sinó que també guardi un historial de cada pas que ha fet.

Tasques:

- Escriu una funció sumarReg que sumi dos enters 
  i registri l'acció com un String en una llista.

- Escriu una funció multReg que multipliqui dos enters i registri l'acció.

- Escriu una funció complexCalc utilitzant la notació do per:


Utilitzar la calculadora per:

- Sumar 3 i 5.

- Multiplicar el resultat per 10.

- Restar 2 al resultat 

- Executa la computació amb runWriter.


----------------------------------------------------------------------------------}


-- Utilitzar el "runWriter" per poder executar les accions, per exemple:
{-
    ghci> runWriter $ sumarReg 3 5
    (8,["Sumar 3 + 5"])
    
    ghci> runWriter $ restarReg 3 5
    (-2,["Restar 3 - 5"])

    ghci> runWriter $ multReg 3 5
    (15,["Multiplicar 3 * 5"])
-}

sumarReg :: Int -> Int -> Writer [String] Int
--sumarReg x y = Writer (x + y, ["Sumar " ++ show x ++ " + " ++ show y])
sumarReg x y = do
    tell ["Sumar " <> show x <> " + " <> show y]
    pure (x + y)

multReg :: Int -> Int -> Writer [String] Int
-- multReg x y = Writer (x * y, ["Multiplicar " ++ show x ++ " * " ++ show y])
multReg x y = do
    tell ["Multiplicar " <> show x <> " * " <> show y]
    pure (x * y)

restarReg :: Int -> Int -> Writer [String] Int
-- restarReg x y = Writer (x - y, ["Restar " ++ show x ++ " - " ++ show y])
restarReg x y = do
    tell ["Restar " <> show x <> " - " <> show y]
    pure (x - y)

complexCalc :: Writer [String] Int
complexCalc = do
    -- Writer es una accio que quan executi retornara un Int
    x1 <- sumarReg 3 5
    x2 <- multReg x1 10
    restarReg x2 2
