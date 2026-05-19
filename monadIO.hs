

{-
Accions com llegir de entrada estàndard, escriure en un fitxer o enviar per una xarxa són 
efectes laterals. Els efectes laterals no es poden representar amb codi funcional pur.
Però si que ès possible representar efectes laterals en un llenguatge pur.
Hi ha diverses maneres de fer-ho, i la manera Haskell és utilitzar Aplicatius i Monads.

A continuació es fa una (molt) breu introduució al monad IO,
que es pot utilitzar en Haskell per a tot tipus d'efectes laterals.

Els tipus de les funcions getLine i putStrLn són:

 - getLine :: IO String

 - putStrLn :: String -> IO ()


Observar que getNum composa 2 accions IO. La segona depen del resultat de la primera. 
Per altra banda, mostrarNums composa 3 accions IO.
-}

getNum :: IO Int -- es una accio que quan s'executi es donara un enter
getNum = do
    s <- getLine
    pure (read s)


mostrarSuma :: IO ()
mostrarSuma = do
    x <- getNum
    y <- getNum
    putStrLn (showSuma x y)



showSuma :: Int -> Int -> String
showSuma x y = show $ x + y


-- Anem a fer l'exemple  més complert:


exempleDiv :: IO ()
exempleDiv = do
    mostrarDivisio getInt

getInt :: IO Int
getInt = do
    putStr "Introdueix un numero: "
    s <- getLine
    pure (read s)

mostrarDivisio :: IO Int -> IO ()
mostrarDivisio gInt = do
    x <- gInt
    y <- gInt
    -- x, y son de tipus Int
    let r :: String
        r = if y == 0 then "No es pot dividir"
            else show (div x y)
    putStrLn ("El resultat de la divisio es : "  ++ r)
