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



-- Dades Servidor

data Servidor = Servidor
        { connectats :: MVar (M.Map Nick Usuari)
        , canalEscriptura :: ChanW MissatgeUsuari
        }

crearServ :: IO Servidor
crearServ = do
    conectats <- newMVar  M.empty
    chanBroadcast <- newBroadcastChanW
    pure $ Servidor conectats chanBroadcast


-- Dades Usuari

data Usuari = Usuari
        { canalLectura :: ChanR MissatgeUsuari
        , nick :: Nick
        , socketUsr :: Socket
        }

type Nick = String

-- Missatges del Servidor cap al client remot
data MissatgeUsuari
    = MissNotif String
    | MissChat Nick String
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
afegirUsuari nom socket servidor = undefined

-- treu l'usuari del mapa del servidor
treureUsuari :: Usuari -> Servidor -> IO ()
treureUsuari usuari servidor = undefined


------------------  Enviar broacast / Rebre missatge client -----------------

-- escriure missatge pel canal de broadcast
broadcast :: MissatgeUsuari -> Servidor -> IO ()
broadcast miss servidor = undefined

-- llegir missatge de l'extrem de lectura de l'usuari
esperarMissatge :: Usuari -> IO MissatgeUsuari
esperarMissatge usuari = undefined


------------------------  Lògica de l'aplicació ---------------------------


servidorChat :: IO ()
servidorChat = do
        let port = 3000 :: PortNumber
        srvrSckt <-  nouServerSocket port
        infoLog $ "Escoltant pel port " <> show port
        servidor <- crearServ
        bucleAccept srvrSckt (tascaChat servidor)


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


-- demana un nick a l'usuari fins que aquest li
-- envia un nick que no estigui sent utilitzat
obtenirNickUnic :: Servidor -> Socket -> IO Usuari
obtenirNickUnic servidor sc = undefined


-- Obtenir Nick (que sera unic) i despres executar tascaBroadcast.
-- Quan la tascaBroadcast acaba s'ha de treure l'usuari de
-- l'estat de la llista de connectats
tascaChat ::  Servidor -> Socket ->IO ()
tascaChat servidor sc  = undefined

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



    -- Crear un altre thread que executa tascaMissPropi
    -- El detall de l'implementació ha de ser de manera que
    -- un cop acabada la tasca es faci un kill del thread
    -- que executa la tasca tascaMissAltres
    

  where
    -- Tasca per esperar llegir missatges que ALTRES usuaris 
    -- han posat al canal de broadcast
    -- i quan arriba un missatge enviar-lo al client remot
    -- i aixi succesivament
    tascaMissAltres = do

 
    -- Tasca que espera rebre missatges del client remot associat
    -- a l'usuari un cop rebut el missatge en fa broadcast
    -- i aixi succesivament fins que es rep /fi i la tasca acaba
    tascaMissPropi = do
        
        



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

