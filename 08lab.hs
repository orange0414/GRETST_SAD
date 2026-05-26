import Control.Concurrent
import Control.Exception
import System.Random
import Control.Monad
import System.IO.Unsafe
import Data.List (delete)
import Network.Socket
-- stack install network -> instalar llibreria
import Network.Socket.ByteString (recv, sendAll)
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.Map as M


controlPrint :: MVar ()
controlPrint = unsafePerformIO $ newMVar ()

atomicPutStrLn ::  String -> IO ()
atomicPutStrLn str = 
            takeMVar controlPrint>> putStrLn str >> putMVar controlPrint ()



---------------------------------------------------------------------------
------------------------------- Threads -----------------------------------
---------------------------------------------------------------------------

-- retardar s segons
retardar :: Int -> IO ()
retardar s = threadDelay $ s*1000000

tascaSegons :: Int -> IO ()
tascaSegons s = do
                atomicPutStrLn  $ "Inici tasca retardar " ++ (show s) ++ " segons."
                retardar s
                

sequencials2 :: IO ()
sequencials2 = do
                    tascaSegons 4
                    putStrLn  "finalitzada tasca 4 segons"
                    do 
                        putStrLn  "10 segons mes"
                        tascaSegons 10 
                        putStrLn  "finalitzada tasca 10 segons mes"
{-  Sortida:
    Inici tasca retardar 4 segons.
    finalitzada tasca 4 segons
    10 segons mes
    Inici tasca retardar 10 segons.
    finalitzada tasca 10 segons mes

    cadascuna retardada els segons corresponents
-}

---------------------------------------------------------------------------
---------------------------- Ús de finally --------------------------------
---------------------------------------------------------------------------

{- finally :: IO a -> IO b -> IO a
    finally
        :: IO a	 -- computation to run first
        -> IO b	 -- computation to run afterward (even if an exception was raised)
        -> IO a	 
    

    Executa accio1
    Passi el que passi, executa accio2
    Si accio1 havia fallat, després de accio2 torna a llençar l'error
-}           

finallySequencials2 :: IO ()
finallySequencials2 = finally   (do
                                    tascaSegons 4
                                    putStrLn  "finalitzada tasca 4 segons"
                                )
                                (do 
                                    putStrLn "10 segons mes"
                                    tascaSegons 10 
                                    putStrLn  "finalitzada tasca 10 segons mes"
                                )
-- es tracta igual de sequencials2

tascaExcepcio ::  IO ()
tascaExcepcio  = do
                error "Excepcio MEVA!!"
                putStrLn $ "NO surt!"


sequencials2Excepcio :: IO ()
sequencials2Excepcio =  do
                            tascaExcepcio
                            do 
                                putStrLn  "10 segons mes"
                                tascaSegons 10 
                                putStrLn  "finalitzada tasca 10 segons mes"
{-  Sortida:
    ghci> sequencials2Excepcio 
    *** Exception: Excepcio MEVA!!
    CallStack (from HasCallStack):
    error, called at /home/orange041/sad/08lab.hs:69:17 in main:Main

    No surt res de la resta ni s'espera fent tasca:
    NO surt!
    10 segons mes
    Inici tasca retardar 10 segons.
    finalitzada tasca 10 segons mes
-}



-- s'executa la segona tasca pero es propaga l'excepcio

finallySequencials2Excepcio :: IO ()
finallySequencials2Excepcio = finally   tascaExcepcio

                                        (do 
                                            putStrLn  "Ara s'executa"
                                            putStrLn  "10 segons mes"
                                            tascaSegons 10 
                                            putStrLn  "finalitzada tasca 10 segons mes"
                                        )
{-  Srtida:
    ghci> finallySequencials2Excepcio 
    Ara s'executa
    10 segons mes
    Inici tasca retardar 10 segons.
    finalitzada tasca 10 segons mes
    *** Exception: Excepcio MEVA!!
    CallStack (from HasCallStack):
    error, called at /home/orange041/sad/08lab.hs:79:17 in main:Main

    es fa la segona accio, i l'error produit en el primer surt seguit a 2a tasca
-}


-- forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinallyExcepcio :: IO ()
forkFinallyExcepcio = do 
                _ <- forkFinally    (tascaExcepcio) 
                                    (\ _ -> do
                                        putStrLn $ "10 segons mes"
                                        tascaSegons 10 
                                        putStrLn $ "finalitzada tasca 10 segons mes"
                                    )                    
                retardar 20
{-  Sortida:
    ghci> forkFinallyExcepcio 
    10 segons mes
    Inici tasca retardar 10 segons.
    finalitzada tasca 10 segons mes

    estem ignorant l'error que surt de tascaExcepcio amb "\ _ -> ..."
    l'accio2 rep "Right resultat" o "Left excepcio"
-}                        


---------------------------------------------------------------------------
------------------------ Finalització de Threads --------------------------
---------------------------------------------------------------------------

finalitzacioThreads :: IO ()
finalitzacioThreads = do
    -- Crear un thread
    idenThread1 <- forkIO $ do
                                tascaSegons 120
                                putStrLn "Fi tascaSegons 120 " 
    -- quan acabi la seguent tasca 
    -- fara finalitzar el thread idenThread1
    finally (do
                tascaSegons 10
                putStrLn "tascaSegons 10 acabada")
            (do
                putStrLn "finalitzar altre thread"
                killThread idenThread1
            )
-- forkIO :: IO () -> IO ThreadId
{-
    es crea un thread nou amb forkIO
    aquest thread comença una tasca de 120 segons
    el thread principal executa una tasca de 10 segons
    quan acaba la tasca de 10 segons, entra al finally
    el finally executa la part final
    aquesta part fa killThread idenThread1
    el thread de 120 segons queda cancel·lat
-}

---------------------------------------------------------------------------
----------------------------- F2.3 Exercic --------------------------------
---------------------------------------------------------------------------
exerciciThreads :: IO ()
exerciciThreads = do
    -- crea un 1r thread en l'espera del text per teclat
    idThread1 <- forkIO $ do
                            putStrLn "Escriu un text:"
                            text <- getLine
                            putStrLn $ "Has escrit: " ++ text

    -- crea un 2n thread que espera 5 segons i mata el primer
    _ <- forkIO $ do
                    retardar 5
                    putStrLn "Han passat 5 segons. Finalitzant el primer thread..."
                    killThread idThread1
    
    pure ()

{-
    Cas de no escriure res:
        ghci> exerciciThreads 
        Escriu un text:
        Han passat 5 segons. Finalitzant el primer thread...

    Cas d'escriure:
        ghci> exerciciThreads 
        Escriu un text:
        hola
        Hghci> as escrit: hola
        Han passat 5 segons. Finalitzant el primer thread...

    quan he escrit algu, es barreja amb la 'ghci>' ja que estan accedint a escriptura de pantalla a l'hora
-}




---------------------------------------------------------------------------
------------------------- Canal de Broadcast ------------------------------
---------------------------------------------------------------------------

type ChanW a = MVar [Chan a]
type ChanR a = Chan a

-- crea un nou canal de broadcast amb l'extrem d'escriptura
newBroadcastChanW :: IO (ChanW a)

-- afegeix un nou extrem de lectura
addChanR :: ChanW a -> IO (ChanR a)
-- treu un nou extrem de lectura
removeChanR :: ChanW a -> ChanR a -> IO ()

-- escriu en l'extrem d'escriptura
writeChanW :: ChanW a -> a -> IO ()
-- llegeix d'un extrem de lectura
readChanR :: ChanR a -> IO a

-- type ChanW a = MVar [Chan a]
newBroadcastChanW = newMVar []

-- treu la llista chrs del chw que li passem
-- crea un chan nou si la llista esta buida
-- si no esta buida copia el primer dels chrs
-- l'afegeix a la llista
addChanR chw = do
    chrs <- takeMVar chw
    chr <- if null chrs then newChan else dupChan (head chrs)
    putMVar chw (chr : chrs)
    pure chr
{-  venen del : import Control.Concurrent.Chan
    ghci> :t dupChan 
    dupChan :: Chan a -> IO (Chan a)
    newChan :: IO (Chan a)
-}

-- 1. modifyMVar_ agafa el contingut de chw
-- 2. chrs és la llista actual de canals lectors
-- 3. delete chr chrs elimina chr de la llista
-- 4. pure retorna la nova llista dins IO
-- 5. modifyMVar_ torna a guardar la llista modificada dins del MVar
removeChanR chw chr = modifyMVar_ chw $ \ chrs ->
    pure $ delete chr chrs

-- llegeix el canal chr
readChanR chr = readChan chr

{-
    1. Agafa la llista de canals lectors del MVar:
    chrs <- takeMVar chw

    2. Mira si hi ha algun canal:
    - Si hi ha almenys un canal: ch : _
        escriu x en aquest canal.

    - Si no hi ha cap canal: []
        no fa res.

    3. Torna a posar la mateixa llista dins del MVar:
    putMVar chw chrs
-}
writeChanW chw x = do
    chrs <- takeMVar chw
    case chrs of
        ch : _ -> writeChan ch x
        [] -> pure ()
    putMVar chw chrs
-- dupChan (head chrs)
-- aquest dupllicat fa que els canals comparteixen el mateix flux
-- escriure en un ja es suficient


{-
    id      -- identificador de l'emissor: 0, 1, ...
    [a]     -- llista de dades a enviar
    ChanW a -- canal de broadcast d'escriptura
-}
emissor ::  (Show a) => Int -> [a] -> ChanW a  ->  IO ()
emissor  id [] chn   = pure ()
emissor  id (x:xs) chn   = do
                temps <- randomRIO (1::Int, 6)
                retardar temps
                writeChanW chn x
                atomicPutStrLn $ "Enviat < " ++ (show id) ++ " > : " ++ show x
                -- loop back
                emissor id xs chn  
{-
emissor id xs chn  = forM_ xs $ \ x -> do
                    temps <- randomRIO (1::Int, 6)
                    retardar temps
                    writeChanW chn x
                    atomicPutStrLn $ "Enviat < " ++ (show id) ++ " > : " ++ show x
-}

-- receptor amb identificador id llegirà n missatges del canal chr
receptor :: (Show a) => Int -> Int -> ChanR a -> IO ()
receptor id 0 _ = pure ()
receptor id n chr = do
    x <- readChanR chr
    atomicPutStrLn $ "Rebut < " ++ show id ++ " > : " ++ show x
    receptor id (n - 1) chr


{-

                                                    
                      bCastChan                      
                 +----------------+ chanR1           
                 |                |+------+          
                 | +------------> || ---> |receptor0 
emissor0 +------+|/               |+------+          
         | ---> ||                |                  
emissor1 +------+|\               |+------+          
                 | +------------> || ---> |receptor1 
                 |                |+------+          
                 +----------------+ chanR2           
-}


provaChan :: IO ()
provaChan = do
    bCastChan <- newBroadcastChanW
    let dades = "abcde"
    chanR1 <- addChanR bCastChan    
    atomicPutStrLn ""
    forkIO $ receptor 0 (2*(length dades)) chanR1   -- 2 vegades logitud de dades es
                                                    -- el total de dades que rebra
    chanR2 <- addChanR bCastChan
    forkIO $ receptor 1 (2*(length dades)) chanR2
    retardar 10
    forkIO $ emissor 0 dades bCastChan
    forkIO $ emissor 1 dades bCastChan
    retardar 25 -- per esperar que acabi i no escriure una linea de ghci> al mig
    pure ()


---------------------------------------------------------------------------
---------------------------  Servei d'Eco ---------------------------------
---------------------------------------------------------------------------

---------------------------------------------------------------------------
------------------------------  Servidor ----------------------------------
---------------------------------------------------------------------------


servidorEco :: IO ()
servidorEco = do
    let port = 3000 :: PortNumber

    -- Creem el socket servidor escoltant al port 3000
    srvrSckt <- nouServerSocket port

    infoLog $ "Escoltant pel port " <> show port

    -- Executem el bucle principal del servidor
    -- Si el servidor finalitza o rep Ctrl+C,
    -- tanquem correctament el socket servidor
    finally
        (bucleAccept srvrSckt tascaEcoServidor)
        (tancarSocket srvrSckt) -- Per tancar el socket del bucleAccpet
        -- quan fem Ctrl + C de l'extrem ServidorEco no es tanca el socket de accept
        --aquest finally serveix per tancar-ho

-- accept :: Socket -> IO (Socket, SockAddr) 
bucleAccept :: Socket -> (Socket -> IO a) -> IO a    
bucleAccept serverSocket tasca  = do 
        (socket, adreRemota) <- accept serverSocket 
        infoLog $ "Connexió acceptada desde " <> show adreRemota 
        -- Crear un thread que executi la tascaChat i que en acabar
        -- la tascaChat tanqui el socket                
        forkFinally (tasca socket) 
                    (\_ -> do
                        infoLog $ "Connexió acabada desde " <> show adreRemota
                        tancarSocket socket
                    )
        bucleAccept serverSocket tasca

tascaEcoServidor :: Socket -> IO ()
tascaEcoServidor sc = do
    -- Enviem el missatge inicial només una vegada
    sendAll sc $ pack "Entra text:\n"
    bucle
  where
    bucle = do
        -- Espera missatge del client
        miss <- recv sc 1024
        let missString = unpack miss

        if missString == "" then
            -- en l'extrem client hem limitat que no envii String buit
            -- per tant si rep "" es perque el client ha fet Ctrl + C
            infoLog $ "Client ha tancat la connexio"
        else
            -- Mostra el missatge al servidor
            infoLog $ "<Rebut> " ++ missString

        -- Retorna el mateix missatge al client
        sendAll sc miss

        -- Si rep /fi / "" acaba, sinó continua
        if missString == "/fi" || missString == "" then do
            infoLog "Acabant connexio ..."
            pure ()
        else 
            bucle

{-
    1. servidorEco obre un socket al port 3000.
    2. bucleAccept espera connexions de clients.
    3. Cada vegada que entra un client, accepta la connexió.
    4. Crea un thread nou per atendre aquest client.
    5. Quan la tasca del client acaba, tanca el socket.
    6. El servidor continua esperant més clients.
-}

{-
    -------------------- Encara existeix PROBLEMA --------------------
    Quan es tanca el servidor "Ctrl + C" i de l'extrem ClientEco encara pot enviar misstges,
    i el servidor continua responent amb el missatge rebut encara que estigui tancat. S'ha de
    tancar des de l'extrem ClientEco enviant "/fi" perquè tot el procés de tancament sigui
    fructuosa.
-}

---------------------------------------------------------------------------
--------------------------  Client Eco ------------------------------------
---------------------------------------------------------------------------


clientEco :: IO ()
clientEco = exeClient "127.0.0.1" "3000" tascaEcoClient

exeClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
exeClient host port tasca =  do
    sck <- obrirSocket host port
    tasca sck

tascaEcoClient :: Socket -> IO ()
tascaEcoClient sc = do
    -- Rep el primer missatge del servidor: "Entra text:"
    queFer <- recv sc 1024
    putStrLn $ unpack queFer

    --cComença el bucle principal del client
    bucle
  where
    bucle = do
        -- llegeix una linia escrita pel client
        putStr "<Client> "
        txt <- getLine

        -- si el clientEco prem Enter sense escriure res,
        -- no enviem res al servidor i tornem a demanar text
        -- es provoca un bloquejament
        if txt == "" then do
            putStrLn "No es pot enviar text buit"
            bucle
        else do
            -- Envia el text escrit al servidor
            sendAll sc (pack txt)
            putStrLn $ "<Missatge enviat al servidor> " ++ txt -- comprova si es va enviar

            -- Espera la resposta del servidor
            miss <- recv sc 1024
            let missRebut = unpack miss

            -- Mostra el missatge retornat pel servidor per comprovar que es rebut
            putStrLn $ "<Misstage del Servidor> " ++ missRebut

            -- Si el missatge és /fi, tanquem el socket i acabem
            -- Si no, continuem demanant més text
            if missRebut == "/fi" then 
                do
                    tancarSocket sc
                    infoLog "Client finalitzat"
                    pure ()
            else 
                bucle
{-
    1. clientEco connecta amb el servidor:
        127.0.0.1 port 3000

    2. exeClient obre el socket i executa tascaEcoClient.

    3. tascaEcoClient entra en un bucle:
    - rep del servidor el text "Entra text: "
    - el mostra per pantalla
    - llegeix text del teclat amb getLine
    - envia aquest text al servidor
    - rep la resposta del servidor
    - mostra "<Rebut> ..."
-}



---------------------------------------------------------------------------
-----------------  Client General Multithread -----------------------------
---------------------------------------------------------------------------

clientGeneral :: IO ()
clientGeneral = exeClient "127.0.0.1" "3000" tascaClient


-- Espera a llegir del socket i 
-- mostra el que ha llegit per pantalla
tascaXarxa :: Socket -> IO ()
tascaXarxa sc = do
    -- Espera a rebre fins a 1024 bytes del socket
    miss <- recv sc 1024

    -- Convertim el missatge rebut a String
    let txt = unpack miss

    -- Si txt és buit, normalment vol dir que el servidor ha tancat connexió
    if txt == "" then 
        pure ()
    else do
        -- Mostra el missatge rebut
        putStrLn $ "<Servidor> " ++ txt
        putStr "<Jo> "

        -- Continua esperant més missatges
        tascaXarxa sc

-- Espera a llegir del teclat i 
-- envia el que s'ha escrit pel socket
tascaTeclat :: Socket -> IO ()
tascaTeclat sc = do
    -- Espera que l'usuari escrigui una línia pel teclat
    txt <- getLine
    if txt == "" then do
        putStrLn "----No es pot enviar text buit----"
        putStr "<Jo> "
        tascaTeclat sc
    else do
        -- Envia el text escrit pel socket
        sendAll sc (pack txt)

    -- Si l'usuari escriu /fi, acaba el bucle del teclat
    -- Si no, continua llegint més text
    if txt == "/fi" then 
        pure ()
    else 
        tascaTeclat sc



-- executa tascaXarxa i tascaTeclat en Threads diferents
tascaClient :: Socket -> IO ()
tascaClient sc = do
    tascaXarxaId <- forkIO $ tascaXarxa sc

    finally
        (tascaTeclat sc)
        (do
            killThread tascaXarxaId
            tancarSocket sc)

{-
    Thread 1: tascaXarxa -> escolta el socket
    Thread principal: tascaTeclat -> llegeix teclat i envia
-}

---------------------------------------------------------------------------
--------------------------  Servidor Eco Nick -----------------------------
---------------------------------------------------------------------------

-- Dades Usuari

data Usuari = Usuari
        { nick :: Nick
        , socketUsr :: Socket
        }

type Nick = String

servidorEcoNick :: IO ()
servidorEcoNick = do
    let port = 3000 :: PortNumber
    srvrSckt <- nouServerSocket port

    infoLog $ "Escoltant pel port " <> show port

    -- Executa el servidor amb nick
    -- Si fem Ctrl+C, es tanca el socket principal del servidor
    finally
        (bucleAccept srvrSckt tascaEcoServidorNick)
        (tancarSocket srvrSckt)


tascaEcoServidorNick :: Socket -> IO ()
tascaEcoServidorNick sc = do
        usuari <- obtenirNick sc
        bucleEco usuari 

obtenirNick :: Socket -> IO Usuari
obtenirNick sc = do
    -- Demanem al client que introdueixi el seu nick
    sendAll sc $ pack "Entra el teu nick: "

    -- Llegim el nick que envia el client pel socket
    nom <- recv sc 1024

    -- Convertim el ByteString rebut a String
    let nickUsuari = unpack nom

    -- Creem i retornem l'usuari amb el nick i el seu socket
    pure $ Usuari nickUsuari sc

bucleEco :: Usuari -> IO ()
bucleEco usuari = do
    -- Demanem text al client només una vegada
    sendAll (socketUsr usuari) $ pack "Entra text"
    bucle
  where
    bucle = do 
        -- Rebem el missatge del client
        miss <- recv (socketUsr usuari) 1024
        let missString = unpack miss

        -- Si recv retorna "", vol dir que el client ha tancat connexió
        if missString == "" then do
            infoLog $ nick usuari ++ " ha tancat la connexio"
            pure ()
        else do
            -- Mostrem al servidor qui ha enviat el missatge
            infoLog $ "<" ++ nick usuari ++ "> " ++ missString

            -- Retornem el missatge al client, afegint el nick
            sendAll (socketUsr usuari) $
                pack $ nick usuari ++ ": " ++ missString

            -- Si el client escriu /fi, acabem la connexió
            -- Si no, continuem escoltant més missatges sense reenviar "Entra text:"
            if missString == "/fi" then do
                infoLog $ "Acabant connexio de " ++ nick usuari
                pure ()
            else bucle



---------------------------------------------------------------------------
------------------  Servidor Eco Nick Unic      ---------------------------
---------------------------------------------------------------------------


-- Dades Servidor

data Servidor = Servidor { connectats :: MVar (M.Map Nick Usuari) }

crearServ :: IO Servidor
crearServ = do
    connectats <- newMVar M.empty
    pure $ Servidor connectats 




------------------------  Gestió d'usuaris ------------------------------------


servidorEcoNickUnic :: IO ()
servidorEcoNickUnic = do
    let port = 3000 :: PortNumber
    srvrSckt <- nouServerSocket port

    infoLog $ "Escoltant pel port " <> show port

    -- Creem l'estat del servidor, amb el mapa d'usuaris connectats
    servidor <- crearServ

    -- Executa el servidor amb nick únic
    -- per a que quan fem Ctrl+C, es tanqui el socket principal del servidor
    finally
        (bucleAccept srvrSckt (tascaEcoServidorNickUnic servidor))
        (tancarSocket srvrSckt)


-- intenta afegir un nou usuari al mapa del servidor
-- La funció té com a resultat una acció que produeix:
--  - Just usuari, si no hi cap usuari amb nick 'nom'
--  - Nothing, si ja hi ha un usuari amb nick 'nom' 
afegirUsuari :: Nick -> Socket -> Servidor -> IO (Maybe Usuari)
afegirUsuari nom socket servidor = do
    -- Agafem el mapa d'usuaris connectats
    mapa <- takeMVar (connectats servidor)

    -- Si el nick ja existeix, tornem a guardar el mapa igual
    if M.member nom mapa then do
        putMVar (connectats servidor) mapa
        pure Nothing

    -- Si el nick no existeix, creem l'usuari i l'afegim al mapa
    else do
        let usuari = Usuari nom socket
        let mapa2 = M.insert nom usuari mapa

        -- Guardem el mapa actualitzat
        putMVar (connectats servidor) mapa2

        -- Retornem l'usuari creat
        pure $ Just usuari



-- treu l'usuari del mapa del servidor
treureUsuari :: Usuari -> Servidor -> IO ()
treureUsuari usuari servidor =
    -- Eliminem del mapa l'usuari segons el seu nick
    modifyMVar_ (connectats servidor) $ \mapa ->
        pure $ M.delete (nick usuari) mapa


-- demana un nick a l'usuari fins que aquest li
-- envia un nick que no estigui sent utilitzat
obtenirNickUnic :: Servidor -> Socket -> IO Usuari
obtenirNickUnic servidor sc = do
    -- Demanem un nick al client
    sendAll sc $ pack "Entra el teu nick: "

    -- Llegim el nick enviat
    nom <- recv sc 1024
    let nickUsuari = unpack nom

    -- Intentem afegir l'usuari al servidor
    resultat <- afegirUsuari nickUsuari sc servidor

    case resultat of
        -- Si el nick no estava ocupat, acceptem l'usuari
        Just usuari -> do
            sendAll sc $ pack "Nick acceptat\n"
            pure usuari

        -- Si el nick ja existia, tornem a demanar-ne un altre
        Nothing -> do
            sendAll sc $ pack "Nick ja utilitzat. Prova un altre.\n"
            obtenirNickUnic servidor sc


-- veure funcio tascaEcoServidorNick 
tascaEcoServidorNickUnic :: Servidor -> Socket -> IO ()
tascaEcoServidorNickUnic servidor sc = do
    -- Primer obtenim un usuari amb nick únic
    usuari <- obtenirNickUnic servidor sc

    -- Executem el bucle d'eco
    -- Quan acabi, eliminem l'usuari del mapa del servidor
    finally
        (bucleEco usuari)
        (treureUsuari usuari servidor)




---------------------------------------------------------------------------
------------------------------  Utilitats ---------------------------------
---------------------------------------------------------------------------


nouServerSocket :: PortNumber -> IO Socket
nouServerSocket port = do
    sock <- socket AF_INET Stream 0   -- create socket
    setSocketOption sock ReuseAddr 1  -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet port 0)   -- listen on the provided TCP port.
    listen sock 2                     -- set a max of 2 queued connections
    pure sock


obrirSocket :: HostName -> ServiceName -> IO Socket
obrirSocket host port = do
        addr <- determinarAdr
        sock <- openSocket addr
        connect sock (addrAddress addr)
        return sock
  where
    determinarAdr = do
        let configuracio = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just configuracio) (Just host) (Just port)


tancarSocket :: Socket -> IO ()
tancarSocket sc = do
    debugLog "Socket tancat"
    gracefulClose sc 5000

infoLog :: String -> IO ()
infoLog str = do
    putStr "INFO: "
    putStrLn str

debugLog :: String -> IO ()
debugLog str =
    when dEBUG $ do
        putStr "DEBUG: "
        putStrLn str

dEBUG :: Bool
dEBUG = True
