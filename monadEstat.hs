import Data.Char
import Data.List
import System.IO
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Traversable
import System.Console.ANSI
-- stack install ansi-terminal
import Control.Monad


-------------------------------------------------------------------
-------------------------------------------------------------------
-----------------------    Joc Mostra     -------------------------
-------------------------------------------------------------------
-------------------------------------------------------------------

data EstatEnd = E {lletra :: Char, numIntents :: Int} deriving Show
-- en el estado guarda
-- letra a adivinar
-- numero de intentos realizados

{-
    Monad Estat per sobre monad IO

    newtype StateT s m a
    A state transformer monad parameterized by:
       - s : The state.
       - m : The inner monad.
    Constructor: StateT (s -> m (a, s))
-}
type Endevinar = StateT EstatEnd IO ()
{-
una acción que modifica o consulta un estado de tipo EstatEnd, 
puede hacer operaciones de entrada/salida IO, 
y no devuelve ningún valor útil, solo ().
-}
-- data Estat e a = Estat (e -> (a, c))
-- EstatEnd -- e
-- IO -- a
-- StateT -- T de transformador


estatEnd = E {lletra = 'h', numIntents = 0}

mostrarIntents :: Int -> String
mostrarIntents 0 = ""
mostrarIntents n = "x " ++ (mostrarIntents (n-1))

jocEnd :: Endevinar
jocEnd = do
    -- per fer operacions IO faig servir lift
    -- lift sirve para meter una acción de la mónada de abajo dentro de la mónada transformada.
    -- putStr "Entra lletra: " -- dona IO (), pero necessitem StateT IO (), afegim lift
    lift (putStr "Entra lletra: ")
    jugada <- lift getLine
    -- get: obte l'estat actual
    est <- get
    if (head jugada == lletra est) then lift (putStrLn "Has guanyat") 
    else do
        -- put modifica l'estat
        put $ E {lletra = lletra est, numIntents = (numIntents est)+ 1} 
        est <- get
        lift (putStrLn ("num intents : "++(mostrarIntents (numIntents est))))
        jocEnd



jugarEnd = runStateT jocEnd estatEnd

{- 
evalStateT :: Monad m => StateT s m a -> s -> m a

    Evaluate a state computation with the given initial state 
    and return the final value, discarding the final state.
-}

jugarEnd' = evalStateT jocEnd estatEnd
-------------------------------------------------------------------
-------------------------------------------------------------------
------------------         Moviment            --------------------
-------------------------------------------------------------------
-------------------------------------------------------------------

-- Pos es (files, columnes)
type Pos = (Int,Int)


incFila (x, y) = (x + 1, y)
decFila (x, y) = (x - 1, y)
incCol (x, y) = (x, y + 1)
decCol (x, y) = (x, y - 1)


anar :: Pos -> IO ()
anar (x,y) = setCursorPosition x y


type Moviment = StateT [Pos] IO ()

{- acccio que:
    - a partir d'una funcio donada,
      obte una nova posicio, 
    - l'afegeix a l'estat
    - dibuixa una 'x' en aquesta nova posicio
 - per aixo: 
   - obte l'estat actual
   - accedeix a l'ultima posicio
   - crea una nova posicio aplicant la funcio
     a l'ultima posicio
   - canvia l'estat afegint la nova posicio
     al final de la llista
   - fa servir la funcio dibuixaMarca per dibuixar
     una 'x' en aquesta ultima posicio
-}
mouIdibuixa :: (Pos-> Pos) -> Moviment
mouIdibuixa canviPos = do
    posicions <- get

    -- 'let' crea variables locales, 
    -- no necesita 'in' pq el proopio 'do' ya indica donde debe existir
    let ultimaPos = last posicions
    let novaPos = canviPos ultimaPos

    -- añadir nueva posicion
    put (posicions ++ [novaPos])

    -- ir a la posicion y marca en la nueva posicion
    lift (dibuixaMarca novaPos)


-- 'd' : opcio dreta
-- 'e' : opcio esquerra
-- 'm' : opcio amunt
-- 'v' : opcio avall
-- en qualsevol altre cas: opcio buida
opcio :: Char -> Moviment
opcio 'd' = mouIdibuixa incCol
opcio 'e' = mouIdibuixa decCol
opcio 'm' = mouIdibuixa decFila
opcio 'v' = mouIdibuixa incFila
opcio _   = return ()


-- Dibuixa una 'x' en la posicio Pos
dibuixaMarca :: Pos -> IO ()
dibuixaMarca pos = do
    anar pos
    putChar 'x'



{-
(>>) :: m a -> m b -> m b

    Sequentially compose two actions, 
    discarding any value produced by the first, 
    like sequencing operators (such as the semicolon) 
    in imperative languages.

    'as >> bs' can be understood as the do expression
    do
        as
        bs
-}

{-
Quins son els tipus de les seguents expressions?:
  . mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
    Sirve para aplicar una acción monádica a cada elemento de una lista, en orden, descartando los resultados.

  - (mapM_ opcio entrada) :: StateT [Pos] IO ()
    aplica opcio a cada carácter de entrada.

  - evalStateT (mapM_ opcio entrada) :: [Pos] -> IO ()
    espera una posicion inicial

  - final :: Moviment


  - clearScreen :: IO ()
-}

-- entrada prova
entrada = "ddddddddddddddvvvvvvvvvveeeeeeeeeeemmmmmmddddddvvvee"

{-
Accio StateT [Pos] IO () que dibuixa el moviment provocat per 
'entrada'
-}
accioMovEntrada :: StateT [Pos] IO ()
accioMovEntrada = mapM_ opcio entrada

{-
Accio IO () que:
    - neteja la pantalla
    - executa accioMovEntrada
-}
dibuixarEntrada :: IO ()
dibuixarEntrada = do
    clearScreen
    evalStateT (accioMovEntrada >> final) [(0,0)]
    -- despues de dibujarlo, final colocara el cursor al final buscando
    -- el maximmo sobre el estado final

{-
Sortida:

 xxxxxxxxxxxxxx
              x
              x
              x
   xxxxxxx    x
   x     x    x
   x     x    x
   x   xxx    x
   x          x
   x          x
   xxxxxxxxxxxx

ghci> 
-}

---------------------------------------------------------------------
-------------------     Funcions utils     --------------------------
---------------------------------------------------------------------


-- accio buida
cap :: Moviment
cap = pure ()


-- accio que situa cal cursor al final 
-- del dibuix
final :: Moviment
final = do
    est <- get
    lift $ anar (maxX est + 2, 0)


-- foldl :: (a -> b -> a) -> a -> [b] -> a
maxX xs = foldl aux ((fst . head) xs) xs
    where aux max (x,y)
            | x > max = x
            | otherwise = max


-------------------------------------------------------------------
-------------------------------------------------------------------
------------------    Moviment Interactiu      --------------------
-------------------------------------------------------------------
-------------------------------------------------------------------

-- prova d'executar el seguent monad i observa els problemes que te
movProva :: Moviment
movProva = do
    jugada <- lift getChar
    opcio jugada
    movProva

provaMovProva = runStateT movProva [(0,0)]



-- donada una llista de Pos,
-- obtenir una accio d'IO que 
-- dibuixa una 'x' en cada una de
-- les posicions de la llista
-- fer servir mapM_
-- mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mostrar :: [Pos] -> IO ()
mostrar xs = mapM_ dibuixaMarca xs

{- acccio que:
    - a partir d'una funcio donada,
      obte una nova posicio, 
    - l'afegeix a l'estat
    - redibuixa tot l'estat, dibuxant
      una 'x' en cada posicio
 - per aixo: 
   - obte l'estat actual
   - accedeix a l'ultima posicio
   - crea una nova posicio aplicant la funcio
     a l'ultima posicio
   - canvia l'estat afegint la nova posicio
     al final de la llista
   - redibuixa la llista de posicions
-}
mouIdibuixaI ::  (Pos-> Pos) -> Moviment -- type Moviment = StateT [Pos] IO ()
mouIdibuixaI canviPos = do
    est <- get
    let lastPos = last est
    let newPos = canviPos lastPos
    put $ (est ++ [newPos])
    est <- get
    lift (mostrar est)



-- 'd' : opcio dreta
-- 'e' : opcio esquerra
-- 'm' : opcio amunt
-- 'v' : opcio avall
-- en qualsevol altre cas: opcio buida
opcioI :: Char -> Moviment
opcioI 'd' = mouIdibuixaI incCol
opcioI 'e' = mouIdibuixaI decCol
opcioI 'm' = mouIdibuixaI decFila
opcioI 'v' = mouIdibuixaI incFila
opcioI _   = return ()



{-
Acció que:
    - Demana un sentit (d,e,v,m,f) a l'usuari
    - neteja la consola
    - afegeix a l'estat la nova posicio
      ( i redibuixa )
    - situa el cursor al final del dibuix
    - torna a demanar un sentit 

sera util:
    when :: Applicative f => Bool -> f () -> f ()
        Conditional execution of Applicative expressions
-}
movimentI :: Moviment
movimentI = do
    jugada <- lift getChar

    when (jugada /= 'f') $ do
        lift clearScreen

        opcioI jugada

        final

        movimentI
    final

-- evaluar l'accio movimentI
moureI :: IO ()
moureI = evalStateT movimentI [(0,0)]