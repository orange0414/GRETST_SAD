import Control.Concurrent
import Control.Concurrent.Chan
import qualified Data.Map as M
import Data.Maybe (isJust)
import System.IO
import Network.Socket
import Control.Monad (when)
import Control.Exception
import Network.Socket.ByteString
import Data.ByteString.Char8 (pack, unpack)
import Data.List (delete)

---------------------------------------------------------------------------
-----------------  Client General Multithread -----------------------------
---------------------------------------------------------------------------

clientGeneral :: IO ()
clientGeneral = exeClient "127.0.0.1" "3000" tascaClient


-- Espera a llegir del socket i 
-- mostra el que ha llegit per pantalla
tascaXarxa :: Socket -> IO ()
tascaXarxa sc = do
    miss <- recv sc 1024
    let txt = unpack miss

    if txt == "" then
        pure ()
    else do
        putStrLn txt
        tascaXarxa sc


-- Espera a llegir del teclat i 
-- envia el que s'ha escrit pel socket
tascaTeclat :: Socket -> IO ()
tascaTeclat sc = do
    txt <- getLine

    if txt == "" then do
        putStrLn "----No es pot enviar text buit----"
        tascaTeclat sc
    else do
        sendAll sc (pack txt)

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

-- Dades Servidor
--  1. La llista d'usuaris connectats (nick -> usuari)
--  2. L'extrem d'escriptura del canal de broadcast
data Servidor = Servidor
        { connectats :: MVar (M.Map Nick Usuari)
        , canalEscriptura :: ChanW MissatgeUsuari
        }

crearServ :: IO Servidor
crearServ = do
    -- Mapa inicial d'usuaris connectats (buit)
    conectats <- newMVar  M.empty
    -- Crear el canal compartit de broadcast
    chanBroadcast <- newBroadcastChanW
    -- Construir l'estructura Servidor
    pure $ Servidor conectats chanBroadcast


-- Dades Usuari
-- Representa un client connectat al servidor
data Usuari = Usuari
        { canalLectura :: ChanR MissatgeUsuari
        , nick :: Nick
        , socketUsr :: Socket
        }

-- Un nick és simplement una cadena de text
type Nick = String

-- Missatges del Servidor cap al client remot
data MissatgeUsuari
    = MissNotif String      -- Missatge de notificació
    | MissChat Nick String  -- Missatge d'un usuari
    deriving (Eq)

instance Show MissatgeUsuari where
  show (MissChat desde miss) = "<" <> desde <> ">: " <> miss
  show (MissNotif miss) = "*** " <> miss



------------------------  Gestió d'usuaris ------------------------------------


-- intenta afegir un nou usuari al mapa del servidor
-- La funció té com a resultat una acció que produeix:
--  - Just usuari, si no hi cap usuari amb nick 'nom'
--  - Nothing, si ja hi ha un usuari amb nick 'nom' 
afegirUsuari :: Nick -> Socket -> Servidor -> IO (Maybe Usuari)
afegirUsuari nom socket servidor = modifyMVar (connectats servidor) $ \m ->
    if M.member nom m
      then pure (m, Nothing)
      else do
        -- Nou extrem de lectura pel canal broadcast
        canal <- addChanR (canalEscriptura servidor)

        -- Crear Usuari
        let usuari = Usuari canal nom socket

        -- Afegir-lo al mapa de connectats
        pure (M.insert nom usuari m, Just usuari)

-- treu l'usuari del mapa del servidor
-- i elimina el seu extrem de lectura del canal broadcast
treureUsuari :: Usuari -> Servidor -> IO ()
treureUsuari usuari servidor = do

    -- Eliminar l'usuari de la llista de connectats
    modifyMVar_ (connectats servidor) $
        pure . M.delete (nick usuari)

    -- Eliminar el seu canal de lectura
    removeChanR (canalEscriptura servidor)
                (canalLectura usuari)

{-
    connectats =
    {
    "Anna" -> ...
    "Joan" -> ...
    "Pere" -> ...
    }

    $ M.delete "Joan"

    {
    "Anna" -> ...
    "Pere" -> ...
    }
-}

------------------  Enviar broacast / Rebre missatge client -----------------

-- escriure missatge pel canal de broadcast
broadcast :: MissatgeUsuari -> Servidor -> IO ()
broadcast miss servidor = writeChanW (canalEscriptura servidor) miss

-- llegir missatge de l'extrem de lectura de l'usuari
esperarMissatge :: Usuari -> IO MissatgeUsuari
esperarMissatge usuari = readChanR (canalLectura usuari)


------------------------  Lògica de l'aplicació ---------------------------


servidorChat :: IO ()
servidorChat = do
        let port = 3000 :: PortNumber
        -- Crear el socket del servidor escoltant pel port 3000
        srvrSckt <-  nouServerSocket port
        infoLog $ "Escoltant pel port " <> show port
         -- Crear l'estat inicial del servidor
        servidor <- crearServ
         -- Acceptar connexions indefinidament
        bucleAccept srvrSckt (tascaChat servidor)


-- accept :: Socket -> IO (Socket, SockAddr) 
-- Bucle principal d'acceptació de connexions
-- Cada client acceptat s'executa en un thread independent
bucleAccept :: Socket -> (Socket -> IO a) -> IO a    
bucleAccept serverSocket tasca  = do 

        -- Espera bloquejat fins que arriba un client
        (socket, adreRemota) <- accept serverSocket 
        infoLog $ "Connexió acceptada desde " <> show adreRemota 
        -- Crear un thread que executi la tascaChat i que en acabar
        -- la tascaChat tanqui el socket                
        forkFinally (tasca socket) 
                    (\_ -> do
                        infoLog $ "Connexió acabada desde " <> show adreRemota
                        tancarSocket socket
                    )
        
        -- Tornar a esperar una nova connexió
        bucleAccept serverSocket tasca


-- demana un nick a l'usuari fins que aquest li
-- envia un nick que no estigui sent utilitzat
obtenirNickUnic :: Servidor -> Socket -> IO Usuari
obtenirNickUnic servidor sc = do

    -- Demanar nick al client
    sendAll sc (pack "Nick: ")

    -- Llegir resposta
    nom <- unpack <$> recv sc 1024

    -- Intentar afegir-lo al servidor
    mUsuari <- afegirUsuari nom sc servidor

    case mUsuari of
        Just usuari -> do
            infoLog $ "Nou usuari: " <> nom
            pure usuari

        Nothing -> do
            sendAll sc (pack "Nick ocupat\n")
            obtenirNickUnic servidor sc

-- Obtenir Nick (que sera unic) i despres executar tascaBroadcast.
-- Quan la tascaBroadcast acaba s'ha de treure l'usuari de
-- l'estat de la llista de connectats
tascaChat :: Servidor -> Socket -> IO ()
tascaChat servidor sc = do

    -- Demanar i registrar un nick únic
    usuari <- obtenirNickUnic servidor sc

    finally
        -- Executar la lògica principal del xat per aquest usuari
        (tascaBroadcast usuari servidor)
        -- Quan acaba el xat, treure l'usuari del servidor
        (treureUsuari usuari servidor)

{-
La tascaBroadcast consisteix en dues tasques concurrents:

  - tascaMissAltres: Tasca per esperar llegir missatges 
                    que ALTRES usuaris  han posat al canal de broadcast
                    i quan arriba un missatge enviar-lo al client remot

  - tascaMissPropi: Tasca que espera rebre missatges del client remot 
                   associat a l'usuari un cop rebut el missatge en 
                   fa broadcast i aixi succesivament fins que es 
                   rep /fi i la tasca acaba
    
Observar que un cop acaba la tascaMissPropi TAMBE ha d'acabar 
la tascaMissAltres
-}
tascaBroadcast :: Usuari -> Servidor -> IO ()
tascaBroadcast usuari servidor = do
    -- Crear un thread que executa tascaMissAltres
    idThreadAltres <- forkIO tascaMissAltres

    -- Crear un altre thread que executa tascaMissPropi
    -- El detall de l'implementació ha de ser de manera que
    -- un cop acabada la tasca es faci un kill del thread
    -- que executa la tasca tascaMissAltres
    finally
        tascaMissPropi
        (killThread idThreadAltres)
        -- Quan l'usuari surt del xat (/fi),
        -- deixem d'escoltar el canal broadcast

  where
    -- Tasca per esperar llegir missatges que ALTRES usuaris 
    -- han posat al canal de broadcast
    -- i quan arriba un missatge enviar-lo al client remot
    -- i aixi succesivament
    tascaMissAltres = do
        -- Espera bloquejat fins que arriba un missatge
        miss <- esperarMissatge usuari

        -- Envia el missatge al client remot
        sendAll (socketUsr usuari) (pack (show miss ++ "\n"))

        -- Tornar a esperar el següent missatge
        tascaMissAltres

 
    -- Tasca que espera rebre missatges del client remot associat
    -- a l'usuari un cop rebut el missatge en fa broadcast
    -- i aixi succesivament fins que es rep /fi i la tasca acaba
    tascaMissPropi = do
        -- Esperar missatge escrit pel client
        miss <- recv (socketUsr usuari) 1024
        let missString = unpack miss

        -- Si rep /fi o el client tanca la connexió, acaba
        if missString == "/fi" || missString == "" then do
            infoLog $ "Acabant connexio de " ++ nick usuari
            pure ()
        else do
            -- Publicar el missatge al canal broadcast pq tots els usuaris el rebin
            broadcast (MissChat (nick usuari) missString) servidor

            -- Continuar llegint més missatges
            tascaMissPropi
        



---------------------------------------------------------------------------
------------------------------  Utilitats ---------------------------------
---------------------------------------------------------------------------


---------------------------  Canal de broadcast  --------------------------

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
newBroadcastChanW = newMVar []

addChanR chw = do
    chrs <- takeMVar chw
    chr <- if null chrs then newChan else dupChan (head chrs)
    putMVar chw (chr : chrs)
    pure chr

removeChanR chw chr = modifyMVar_ chw $ \ chrs ->
    pure $ delete chr chrs

readChanR chr = readChan chr

writeChanW chw x = do
    chrs <- takeMVar chw
    case chrs of
        ch : _ -> writeChan ch x
        [] -> pure ()
    putMVar chw chrs


------------------------------------------------------------------------

nouServerSocket :: PortNumber -> IO Socket
nouServerSocket port = do
    sock <- socket AF_INET Stream 0   -- create socket
    setSocketOption sock ReuseAddr 1  -- make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet port 0)   -- listen on the provided TCP port.
    listen sock 2                     -- set a max of 2 queued connections
    pure sock

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

exeClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
exeClient host port tasca = do
    sck <- obrirSocket host port
    tasca sck

------------------------- proves --------------------------
