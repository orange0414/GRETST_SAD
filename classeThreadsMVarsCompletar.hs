import Control.Concurrent
import Text.Printf
import Control.Monad


-- Per a crear un fil
-- forkIO :: IO () -> IO ThreadId
-- IO () -> accio IO


primerAB :: IO ()
primerAB = do
    forkIO $ putStrLn "A" -- crea un fil nou que escriu A
    forkIO $ putStrLn "B" -- crea un fil nou que escriu B
    pure ()


bucleMostrar :: Int -> String -> IO ()
bucleMostrar 0 _ = pure ()
bucleMostrar n str = do
    putStr str
    bucleMostrar (n - 1) str
-- bucleMostrar n str = replicateM_ n (putStrLn str)

segonAB :: IO ()
segonAB = do
    forkIO $ bucleMostrar 10 "hola"
    forkIO $ bucleMostrar 10 "mon"
    pure ()



{-
Les MVar són el mecanisme bàsic de comunicació que ofereix Haskell,
és a dir, les variables globals bàsiques.

data MVar a = ...

newEmptyMVar :: IO (MVar a) 

    - una nova variable compartida entre fils sense valor
 
newMVar :: a -> IO (MVar a)

    - var compartida amb un valor inicial

takeMVar :: MVar a -> IO a

    - Retorna el contingut de l'MVar. 
    
    - Si l'MVar està buida, takeMVar esperarà fins que estigui plena.
      Després d'un takeMVar, l'MVar es queda buida.

putMVar :: MVar a -> a -> IO ()

    - Posa un valor a un MVAR. 
    
    - Si l'MVar és plena, putMvar esperarà fins que es buidi.


Propietats de takeMVar i putMVar:
        
        - Només desperten UN ÚNIC thread. És a dir, si hi ha diversos 
          fils bloquejats a putMVar/takeMVar al canviar l'estat només 
          se'n despertarà un. 
        
        - Quan es bloquegen diversos fils en un MVAR, 
          es desperten en ordre FIFO.


readMVar :: MVar a -> IO a

    - Llegeix atòmicament el contingut d'una MVAR. 
    
    - Si l'MVar és buida, readMVar esperarà fins que estigui plena. 

    - Si hi ha varis threads bloquejats en una MVar, 
      readMVar els despertarà tots al mateix temps.


- Les MVAR són variables que poden estar buides o  contenir un valor.

- Les operacions de posar i treure el valor són bloquejants.

- Les operacions sempre retornen valors del monad IO. 
-}

-- MVar per a compartir valors entre fils

pasValors :: IO ()
pasValors = do
    m <- newEmptyMVar

    forkIO $ do
        putStrLn "Poso valor"
        putMVar m 0

    forkIO $ do
        valor <- takeMVar m
        putStrLn ("Valor rebut: " ++ show valor)

    pure ()
{-
pasValors = do
    mv <- newEmptyMVar
    forkIO $ do
        putMVar mv 0
    r <- takeMVar mv
    print r 
    r <- takeMVar mv
    print r
-}

-- *** Exception: thread blocked indefinitely in an MVar operation

tercerAB :: IO ()
tercerAB = do
    m <- newEmptyMVar

    forkIO $ do
        putStrLn "A"
        putMVar m ()

    forkIO $ do
        takeMVar m
        putStrLn "B"

    pure ()

tercerAB' :: IO ()
tercerAB' = do
  forkIO $ bucleMostrar 1000 "A"
  forkIO $ bucleMostrar 1000 "B"
  putStrLn "acabat"
  pure ()
    


{-
Fer que el fil principal s'esperi a que la resta de fils acabin. 
La idea es disposar d'una MVar on cada fil posa un valor un cop 
ha acabat la seva feina i el fil principal va obtenint valors 
d'aquesta MVar, tants com fils s'han iniciat.
-}

type CF = MVar () -- alies de tipus -> escriure CF es igual que escriure MVar ()
-- CF : Control de Fils


iniciarFil :: IO () -> IO CF
iniciarFil accio = do
    cF <- newEmptyMVar

    forkIO $ execAccio cF
    pure cF
    where 
        execAccio cF = do
            _ <- accio
            putMVar cF ()


--  where
    -- Delega l'execució de la funció del fil a una nova funció, 
    -- que executa la funció del fil i després actualitza l'MVar.



esperarFil :: CF -> IO ()
esperarFil = takeMVar


{-
forM :: Monad m => [a] -> (a -> m b) -> m [b]
    - Map each element of a structure to a monadic action, 
    - evaluate these actions from left to right, 
    - and collect the results. 
-}
-- [IO ()] -> (IO () -> IO CF) -> IO [CF]
iniciarFils :: [IO ()] -> IO [CF]
iniciarFils accions = forM accions iniciarFil
-- iniciarem cada accio IO () de accions amb iniciarFil


{-
replicate :: Int -> a -> [a]
    
    - replicate n x 
      
      is a list of length n with x the value of every element.
-}
iniciarFilsIguals :: Int -> IO () -> IO [CF]
iniciarFilsIguals n accio = iniciarFils $ replicate n accio
-- amb replicate es crea una llista de n accions iguals



{-
forM_ :: Monad m => [a] -> (a -> m b) -> m ()
    - Map each element of a structure to a monadic action, 
    - evaluate these actions from left to right, 
    - and ignore the results. 
-}
esperarFils :: [CF] -> IO ()
esperarFils fils = forM_ fils esperarFil
-- per cada fil fa una esperarFil



meuFun :: String -> IO ()
meuFun str = do
    putStrLn str 
-- rep un Striing i retorna una accio que es printear aquest String


primerFils :: IO ()
primerFils = do
    fils <- iniciarFilsIguals 5 (meuFun "hola")

    esperarFils fils
    putStrLn "Adeu"
-- Aqui nomes garantim que el fil principal pot esperar fins que acaben els fils secundaris



type ControlPrint = MVar ()

atomicPutStrLn :: ControlPrint -> String -> IO ()
atomicPutStrLn lock str = do
    takeMVar lock -- intenta agafar valor de lock, que es un ControlPrint = MVar ()
    putStrLn str
    putMVar lock ()
            


segonFils :: IO ()
segonFils = do
    lock <- newMVar () -- crear una variable plena

    fils <- iniciarFilsIguals 5 (atomicPutStrLn lock "hola")

    esperarFils fils
    putStrLn "Adeu"



{-
Problema clàssic:

  'numFils' fils incrementant una variable 'numInc' cops cada fil. 

Resoldre el problema posant aquesta variable en una MVar.
-}

type Comptador = MVar Int

inc :: Comptador -> IO ()
inc c = do
    num <- takeMVar c
    putMVar c (num + 1)



filInc :: Int -> Comptador -> IO ()
filInc numInc c = replicateM_ numInc (inc c)



exempleComptador :: Int -> Int -> IO ()
exempleComptador numFils numInc = do
    c <- newMVar 0

    fils <- iniciarFilsIguals numFils (filInc numInc c)
    esperarFils fils

    resultat <- takeMVar c 

    putStrLn ("Resultat: " ++ show resultat)





-- Productors - Consumidors

{-
when :: Applicative f => Bool -> f () -> f ()

Execució condicional d'expressions Applicative. 

Exemples:
    
    when debug (putStrLn "Debugging")

  obtindrà a la sortida:
      -  l'string Debugging si el valor booleà 'debug' és True
      -  sino no és farà res.

ghci> putStr "pi:" >> when False (print 3.14159)
pi:

ghci> putStr "pi:" >> when True (print 3.14159) 
pi:3.14159

-}





data BufferPC a = B {cua :: (MVar ([a], Int)), control :: ControlPC}
data ControlPC = C {cp :: MVar (), cc::MVar ()}
-- cp -> inicialment Producer pot posar coses
-- cc -> inicialment Consumidor no pot agafar coses

controlPC :: IO ControlPC
controlPC = do
    cp <- newMVar ()
    cc <- newEmptyMVar
    
    pure $ C cp cc



nouBufferPC :: Int -> IO (BufferPC a)
nouBufferPC capacitat = do
    cua <- newMVar ([], capacitat)
    control <- controlPC

    pure $ B cua control


-- amb el if sempre ha d'haver un else, pero amb el when no fa falta
posarPC :: Show a => ControlPrint -> BufferPC a -> a -> IO ()
posarPC cprt buf val = do
    atomicPutStrLn cprt "Producer vol posar"

    -- Esperar fins que hi hagi espai al buffer
    takeMVar (cp (control buf))

    atomicPutStrLn cprt ("Producer posant: " ++ show val)

    -- Agafar el buffer
    (b, capacitat) <- takeMVar (cua buf)

    -- Afegir el nou element
    let b2 = val : b

    -- Si encara queda espai, permetre que el productor segueixi posant
    when (length b2 < capacitat) $
        putMVar (cp (control buf)) ()

    -- Si abans estava buit, despertar el consumidor
    when (null b) $
        putMVar (cc (control buf)) ()

    -- Retornar el buffer actualitzat
    putMVar (cua buf) (b2, capacitat)





treurePC :: Show a => ControlPrint -> BufferPC a -> IO a
treurePC cprt buf = do
    -- Mostrar que el consumidor vol treure un element
    atomicPutStrLn cprt "Consumidor vol treure"

    -- Esperar fins que hi hagi almenys un element al buffer
    -- Si el buffer està buit, el consumidor queda bloquejat aqui
    takeMVar (cc (control buf))

    -- Agafar el contingut actual del buffer
    (b, capacitat) <- takeMVar (cua buf)

    -- Com que els elements s'afegeixen pel davant (val:b),
    -- l'element més antic està al final de la llista
    let val = last b

    -- Eliminar l'element extret del buffer
    let b2 = init b

    -- Mostrar quin element s'ha consumit
    atomicPutStrLn cprt ("Consumidor treu: " ++ show val)

    -- Si encara queden elements al buffer,
    -- permetre que el consumidor pugui continuar traient
    when (not (null b2)) $
        putMVar (cc (control buf)) ()

    -- Si abans el buffer estava ple,
    -- ara s'ha alliberat una posició i el productor pot continuar posant
    when (length b == capacitat) $
        putMVar (cp (control buf)) ()

    -- Guardar el buffer actualitzat
    putMVar (cua buf) (b2, capacitat)

    -- Retornar l'element consumit
    return val




elems :: [Char]
elems = ['a','b','c','d']

productor :: Show a => ControlPrint -> [a] -> BufferPC a -> IO ()
productor cprt xs buf = do
    mapM_ (posarPC cprt buf) xs




consumidor :: Show a => ControlPrint -> Int -> BufferPC a -> IO ()
consumidor cprt n buf = do
    mapM_ (\_ -> treurePC cprt buf) [1..n]



exemplePC :: IO ()
exemplePC = do
    cprt <- newMVar ()
    buf <- nouBufferPC 2

    let accionsPC = [ productor cprt elems buf, consumidor cprt (length elems) buf]

    fils <- iniciarFils accionsPC
    esperarFils fils

    
    

