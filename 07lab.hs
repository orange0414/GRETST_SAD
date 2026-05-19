import Data.List
import Control.Monad -- ap
import System.Random
-- stack install random 
       
------------------------------------------------------------------------------
----------------------------  Monad  EstatIO --------------------------------- 
------------------------------------------------------------------------------

newtype EstatIO s a = EstatIO { execEstatIO :: s -> IO (s, a) }

instance Functor (EstatIO s) where
    fmap f eiox = EstatIO (\s -> do  -- monad IO
                                    (estat', res) <- execEstatIO eiox s
                                    pure (estat', f res))
    


instance Applicative (EstatIO s) where
    pure a = EstatIO $ \s -> pure (s, a)
    (<*>) = ap

{-
Executem eio amb l’estat inicial s.
Obtenim un nou estat estat1 i un resultat res1.
Apliquem k res1, que dona una nova acció EstatIO.
Executem aquesta nova acció amb l’estat actualitzat estat1
-}
instance Monad (EstatIO s) where
    return = pure
    eio >>= k = EstatIO $ \s -> do 
        (estat1, res1) <- execEstatIO eio s 
        execEstatIO (k res1) estat1


-- obtenir l'estat
obtenirEstatIO :: EstatIO e e
obtenirEstatIO = EstatIO $ \s -> pure (s, s)


-- canviar l'estat
canviarEstatIO :: e -> EstatIO e ()
canviarEstatIO est = EstatIO $ \_ -> pure (est, ())

{-
Utilitzar accions IO en el monad EstatIO :
Construeix una accio EstatIO que quan s'executi
produira el valor que produeix l'accio IO a l'entrada 
-}
pujarIO :: IO a -> EstatIO b a 
pujarIO io = EstatIO $ \estat -> do  -- monad IO
                                  x <- io
                                  pure (estat, x)



------------------------------------------------------------------------------
-----------------------------  Exercici Prova -------------------------------- 
------------------------------------------------------------------------------

{-
Estat prova te un unic constructor amb 2 parametres:
    - una llista d'Strings
    - un enter
-}
data EstatProva = EP {entrades :: [String], numEntrades :: Int}


{--
Accio que demana a l'usuari que entri una linia de text i la guarda en l'estat.
El numero de peticions de linia ve determinat per valor enter en l'estat.
Cada vegada que l'usuari entra una linia es guarda en l'estat i es decrementa
l'enter.
Quan l'enter val zero es mostren totes les entrades que ha fet l'usuari i acaba 
l'accio.
--}
provaEstatIO :: EstatIO EstatProva ()
provaEstatIO = do
    estat <- obtenirEstatIO

    if numEntrades estat == 0
    then pujarIO $ print (entrades estat) -- print es de IO, fem servir pujarIO perque sigui EstatIO
    else do
        linea <- pujarIO getLine

        let novesEntrades = (entrades estat) ++ [linea] -- variable local novesEntrades
        let nouNum = numEntrades estat - 1 -- variable local nouNum, una linea menys
        let nouEstat = EP novesEntrades nouNum

        canviarEstatIO nouEstat
        provaEstatIO


executarProva :: IO (EstatProva, ())
executarProva = execEstatIO provaEstatIO (EP [] 3) -- estat inicial buit, accepta 3 lineas
{-
    ghci> executarProva 
    hola
    mon
    !
    ["hola","mon","!"]
-}



------------------------------------------------------------------------------
----------------------------  Estat del Joc  --------------------------------- 
------------------------------------------------------------------------------

data EstatBD = EBD { secret :: [Integer], 
                     intentsQueden :: Integer,
                     jugades :: [[Integer]] }



------------------------------------------------------------------------------
-------------------------  Algorismica del Joc  ------------------------------ 
------------------------------------------------------------------------------
                  

-- Comprova quants numeros ha posat el jugador en la 
-- posicio correcta
numeroBons :: [Integer] -> EstatIO EstatBD Integer
numeroBons jugada = do
    estat <- obtenirEstatIO

    let parelles = zip jugada (secret estat)
    let bons = [x | x <- parelles, fst x == snd x] 
    -- agafa només les parelles on el primer valor i 
    -- el segon valor són iguals
    pure (fromIntegral (length bons))
{-
    jugada = [5,4,0,8]
    secret = [5,6,0,4]

    parelles = [(5,5),(4,6),(0,0),(8,4)]

    bons = [(5,5),(0,0)]

    length bons = 2

    fromIntegral perque passi de Int a Integer
-}


{--
Numero de digits comuns entre el numero secret i el numero que ha
entrat el jugador, es a dir, numero de digits que el jugador ha 
endevinat independentment de si estan a la posicio correcta.

Se suposa que el secret te tots els digits diferents
i que la jugada tambe te tots els digits diferents
--}
numeroComuns :: [Integer] -> EstatIO EstatBD Integer
numeroComuns jugada = do
    estat <- obtenirEstatIO

    let secretActual = secret estat
    let comuns = [x | x <- jugada, elem x secretActual]
    {-
    recorre cada dígit x de jugada i només guarda els que també apareixen al secret.
    Exemple:

    jugada = [5,4,0,8]
    secretActual = [5,6,0,4]
    -}
    pure (fromIntegral (length comuns))



-- Comprova quants digits ha introduit el jugador en una posicio incorrecta
-- Observar que es pot implementar fent servir les funcions
-- numeroBons i numeroComuns
numeroDolents :: [Integer] -> EstatIO EstatBD Integer
numeroDolents jugada = do
    bons <- numeroBons jugada
    comuns <- numeroComuns jugada
    pure (comuns - bons)

-- Afegeix una jugada a l'estat i DECREMENTA el numero d'intents que queden
-- El valor que produeix es el numero d'intents que queden
afegirJugada :: [Integer] -> EstatIO EstatBD Integer
afegirJugada jugada = do
    estat <- obtenirEstatIO
    -- afegir jugada
    let novaJugada = (jugades estat) ++ [jugada] 
    -- un intent menys
    let intentsRestants = (intentsQueden estat) - 1 
    -- nou estat a partir dels nous parametres
    let nouEstat = EBD (secret estat) intentsRestants novaJugada 
    -- canviar estat
    canviarEstatIO nouEstat
    -- intents que queden, el intentsRestants que retorna ja es Integer
    pure intentsRestants


{-
Evalua una jugada i produeix un boela que indica si el joc ha de continuar
o aquesta es l'ultima jugada.
Una jugada sera l'ultima si el jugador ha endevinat el secret (en aquest guanya),
o be si al jugador ja no li queden intents.

-}
evaluarJugada :: [Integer] -> EstatIO EstatBD  Bool
evaluarJugada jugada = do
    bons <- numeroBons jugada
    dolents <- numeroDolents jugada
    intentsRestants <- afegirJugada jugada

    if bons == 4
        then do
            pujarIO $ putStrLn ""
            mostrarResultats "Has guanyat!"
            pure False
        else if intentsRestants == 0
            then do
                pujarIO $ putStrLn ""
                mostrarResultats "Has perdut!"
                pure False
            else do
                pujarIO $ putStrLn ""
                mostrarResultats ("Bons: " ++ show bons ++ 
                                  " Dolents: " ++ show dolents ++ 
                                  " Intents restants: " ++ show intentsRestants)
                pure True
{-
    Si hi ha 4 bons:
    el jugador ha guanyat
    el joc acaba (False)
    Si no queden intents:
    el joc acaba (False)
    En qualsevol altre cas:
    el joc continua (True)
-}

{-
Accio que un cop s'executa mostra per pantalla l'String que te com a parametre
i tambe mostra totes les jugades previes que el jugador ha fet
-}
mostrarResultats :: String -> EstatIO EstatBD  ()
mostrarResultats missatge = do
    estat <- obtenirEstatIO

    -- pujarIO perque putStrLn es una accio IO
    -- msotra el misstage que ens passa
    pujarIO $ putStrLn missatge 
    -- pujarIO pq mostrarLlistaLlistes utilitza print i tambe es IO
    -- mostra totes les jugades
    pujarIO $ mostrarLlistaLlistes (jugades estat)


------------------------------------------------------------------------------
-----------------------  Interaccio amb l'usuari  ---------------------------- 
------------------------------------------------------------------------------



-- Produeix True si la llista de digits enters te mida 4
-- i tots els digits son diferents
validarLListaDigits :: [Integer] -> EstatIO EstatBD Bool
validarLListaDigits entrada = pure (length entrada == 4 && totsDiferents entrada) 


{--
Obte un numero del jugador consistent en una llista de digits diferents 
de longitud 4. Si el numero que entra el jugador no es aixi, s'indicara 
al jugador que el numero es dolent i que n'entri un altre
--}
obtenirJugada :: EstatIO EstatBD [Integer]
obtenirJugada = do
    pujarIO $ putStrLn ""
    pujarIO $ putStr "Introdueix una jugada (4 numeros diferents): "

    entrada <- pujarIO getLine

    -- strEnter transforma un string en una llista d'enters
    -- ghci> :t strEnter
    -- strEnter :: String -> [Integer]
    let jugada = strEnter entrada

    valida <- validarLListaDigits jugada

    if valida
        then pure jugada
        else do
            pujarIO $ putStrLn "ERROR: Numero amb format incorrecte!"
            obtenirJugada

{--
Obte una jugada valida i l'avalua. Aquestes accions es van repetint
fins que evaluarJugada produeix False.
--}
bucleJugar :: EstatIO EstatBD ()
bucleJugar = do
    jugada <- obtenirJugada
    continuar <- evaluarJugada jugada

    if continuar
        then bucleJugar
        else pure ()


{--
El joc consisteix en generar aleatoriament  un secret inicial que es un numero 
de 4 xifres diferents. Posar aquest numero en l'estat del joc i demanar al 
jugador que jugui 
--}
joc :: EstatIO EstatBD ()
joc = do
    secretInicial <- pujarIO generarSecret
    canviarEstatIO secretInicial
    bucleJugar




jugar :: IO (EstatBD, () )
jugar = execEstatIO joc (EBD [] 0 [])





------------------------------------------------------------------------------
------------------------  Generar numero secret ------------------------------ 
------------------------------------------------------------------------------


generarSecret :: IO EstatBD
generarSecret = do
  num <- randomRIO(1023, 9678)
  if totsDiferents (enterAllista num)
     then
       return $ EBD (enterAllista num) 10 []
     else
       generarSecret


------------------------------------------------------------------------------
-----------------------------  Funcions utils -------------------------------- 
------------------------------------------------------------------------------


-- retorna cert si tots els digits son diferents
-- nub :: Eq a => [a] -> [a]
-- nub treu els duplicats d'una llista
totsDiferents :: [Integer] -> Bool
totsDiferents xs = nub xs == xs


-- converteix una llista de caracters numerics
-- a una llista de numeros
-- read :: Read a => String -> a
-- (:[]) :: a -> [a]
strEnter :: String -> [Integer]
strEnter = map (read . (:[]))

-- converteix un numero enter a una llista dels
-- seus digits
enterAllista :: Integer -> [Integer]
enterAllista 0 = []
enterAllista num = enterAllista (num `div` 10) ++ [num `mod` 10]

-- Mostra una llista de llistes, una llista a cada linia
mostrarLlistaLlistes :: Show a => [[a]] -> IO ()
mostrarLlistaLlistes  =  (mapM_ print)  







