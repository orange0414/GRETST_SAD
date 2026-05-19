import Data.List
import Data.Maybe
import Data.Char
import Control.Applicative
---------------------------------------------------------------------------------------------------------------------
----------------------------------------------05lab------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------
import Data.List
import Data.Maybe
import Data.Char
import Control.Applicative


-------------------------------------------------------------------------


type NumSeq = Int

type CheckSum = [Int]

data TipusSeg = PSH | ACK | SYN | FIN deriving (Show, Eq, Ord)

data Capçalera = Capçalera TipusSeg NumSeq CheckSum deriving (Show, Eq, Ord)

data Dades = Dades [Int] deriving (Show, Eq, Ord)

data TCPSegment  = TCPSegment Capçalera Dades deriving (Show, Eq, Ord)



-- segments rebuts en format binari en una transmissio TCP corresponents a un missatge de text
-- hi ha segments de tipus SYN, PSH, ACK i FIN
-- hi pot haver segments que contenen errors
-- hi pot haver segments duplicats
-- poden estar desordenats
segmentsRebuts :: [[Int]]
segmentsRebuts =
    [[0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,1,0,0,1,1,1,0,0,1,0,1,1,1,0,0,0,1,0,0,0,0,0,0,1,0,0,1,0,0,0,0,1,1,0,0,0,0,1],
    [0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,1,1,0,1,0,0,1,0,1,0,0,1,1,0,0,0,0,1,0,0,1,0,0,0,0,0,0,1,1,1,0,0,0,0],
    [0,0,0,0,0,0,0,1,1,1,1,0,1,0,1,1,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,1,0,0,1,0,0,0,0,1,1,0,1,1,1,1,0,1,1,1,0,1,0,0,0,1,1,1,0,0,1,1,0,0,1,0,0,0,0,0],
    [0,0,0,0,0,0,0,1,0,1,0,1,0,0,1,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,1,0,0,1,0,1,0,0,0,0,0,0,1,1,0,1,0,1,1,0,0,1,1,0,1,1,1,0,1,0,1,1,0,1,1,0,0,1,1,1,0,1,1,1,0,0,0,1],
    [0,0,0,0,0,0,0,0,1,0,0,1,1,0,0,0,0,1,0,1,0,1,0,1,0,1,1,1,0,1,0,0,0,0,1,0,0,0,0,1],
    [0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,1,0,1,1,0,1,1,0,1,0,0,1,0,1,1,1,0,1,0,1,0,1,1,1,0,0,1,0,0,1,1,0,0,1,0,1],
    [0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,0,0,0,1,1,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,0,1,1,0,1,0,0,1],
    [0,0,0,0,0,0,0,1,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,1,1,0,1,0,0,1,1,1,0,0,1,1,0,1,0,0,1,0,1,1,0,0,0,0,0,0,1,1,1,0,1,0,0],
    [0,0,0,0,0,0,0,0,1,0,0,0,1,1,0,0,0,1,0,1,0,0,1,0,0,1,1,1,0,0,1,1,0,0,1,0,0,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,0,1,0,1],
    [0,0,0,0,0,0,0,0,1,0,0,0,1,1,0,0,0,1,0,1,0,0,1,0,0,1,1,1,0,0,1,1,0,0,1,0,0,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,0,1,0,1],
    [0,0,0,0,0,0,0,1,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,0,1,1,1,1,0,0,0,0,1,1,1,1,0,1,1,0,1,0,0,1,0,1,0,0,1,1,0,0,0,0,1,0,0,1,0,0,0,0,0,0,1,1,1,0,0,0,0],
    [0,0,0,0,0,0,0,0,1,0,0,1,0,1,0,0,0,0,0,0,1,1,0,1,0,1,1,0,0,1,1,0,0,1,1,0,1,0,0,1,0,1,1,0,0,0,1,1,0,1,1,0,0,0,0,1],
    [0,0,0,0,0,0,1,1,1,1,1,0,1,0,1,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,0,1,0,0,1,1,1,0,0,1,0,1,1,1,0,1,0,1,0,0,0,0,0,1,1,0,1,1,1,0,0,0,1,1,1,1,0,0,1],
    [0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,0,0,0,1,1,0,1,1,0,1,1,1,1,0,1,1,0,0,1,0,0,0,1,1,0,1,0,0,1],
    [0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1,0,0,0,1,1,0,0,1,0,1,1,1,1,1,1,1,0,1,1,1,0,0,1,0,0,1,0,0,1],
    [0,0,0,0,0,0,0,0,1,0,0,1,0,1,0,0,0,0,0,0,1,1,0,1,0,1,1,0,0,1,1,0,0,1,1,0,1,0,0,1,0,1,1,0,0,0,1,1,0,1,1,0,0,0,0,1],
    [0,0,0,0,0,0,1,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0],
    [0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,1,1,1,0,0,1,1,0,1,1,0,1,1,1,1,0,1,1,0,1,1,0,1,0,1,1,1,0,0,1,0]]

------------------------------------------------------------------------------
------------------- Funcions auxiliars per tractar bits ----------------------
------------------------------------------------------------------------------

-- converteix un enter positiu 
-- a la seva representacio binaria 
-- [b_0, b_1, ... , b_{n-2}, b_{n-1}]
-- on b_{n-1} es el bit menys significaciu
intAbit :: Int -> [Int]
intAbit 0 = []
intAbit n = let r = n `mod` 2
            in (intAbit $ n `div` 2) ++ [r]

-- si la mida de la llista d'entrada es:
--  - mes gran de 8 es queda amb els 8 bits menys significatius
--  - altrament omple amb zeros els bits mes significatius fins 
--    arribar a 8
mida8 :: [Int] -> [Int]
mida8 xs
    | length xs > 8  = drop ((length xs) - 8) xs
    | length xs == 8 = xs
    | otherwise      = mida8 (0:xs)

-- converteix un enter positiu menor que 255 
-- a la seva representacio binaria de 8 bits
-- [b_0, b_1, b_2, b_3, b_4, b_5, b_6, b_7]
-- on b_7 es el bit menys significaciu
--
--    ghci> intA8bit 5  
--    [0,0,0,0,0,1,0,1]
intA8bit :: Int -> [Int]
intA8bit = mida8 . intAbit


-- converteix la representacio binaria d'un enter positiu
-- [b_0, b_1, b_2, b_3, b_4, b_5, b_6, b_7]
-- on b_7 es el bit menys significaciu
-- a l'enter
--
-- ghci> vuitBitaInt [0,0,0,0,0,1,0,1]
-- 5
vuitBitaInt :: [Int] -> Int
vuitBitaInt = foldl (\a b -> 2*a + b) 0

-- Converteix un caracter en el valor binari del
-- seu codi ASCII 
-- [b_0, b_1, b_2, b_3, b_4, b_5, b_6, b_7]
-- on b_7 es el bit menys significaciu
-- fromEnum dona el valor ASCII decimal del caracter
charA8bit :: Char -> [Int]
charA8bit = intA8bit . fromEnum


-- Converteix un valor de codi ASCII expressat com 
-- [b_0, b_1, b_2, b_3, b_4, b_5, b_6, b_7] 
--on b_7 es el bit menys significaciu al seu caràcter corresponent.

bit8Achar :: [Int] -> Char
bit8Achar = chr . foldl (\a b -> 2*a + b) 0

-- Converteix una llista de caràcters a una llista de bits codificant cada caràcter
-- amb la representació binaria del seu codi ASCII

stringA8bit :: [Char] -> [Int]
stringA8bit = concat . map charA8bit 


-- Separa una llista en varies llistes de mida 8. Si la longitud no es multiple de 8 
-- afegeix zeros a l'ultima llista fins a arribar a mida 8

parts8bits :: [Int] -> [[Int]]
parts8bits xs
   | length xs <= 8 = [mida8 xs]
   | otherwise      = (take 8 xs):(parts8bits (drop 8 xs))

-- Donades dues llistes d'enters d'igual longitud, interpreta cada element com un bit, 
-- i fa la suma bit a bit.

sumBitaBit :: [Int] -> [Int] -> [Int]
sumBitaBit [] _ = []
sumBitaBit (x:xs) (y:ys) = ((x+y) `mod` 2):(sumBitaBit xs ys) 

-- Donada una llista de llistes d'enters d'igual longitud, interpreta cada element 
-- com un bit, i fa una suma binària per posicions.

sumarBitaBit :: [[Int]] -> [Int]
sumarBitaBit xs = foldr (\ys zs -> sumBitaBit ys zs) (replicate (length (xs!!0)) 0) xs 


------------------------------------------------------------------------------
---------------------------- REALITZACIO -------------------------------------
------------------------------------------------------------------------------

-- retorna els n primers elements d'una llista
agafa :: Int -> [a] -> [a]
agafa 0 _ = []
agafa _ [] = []
agafa n (x:xs) = x : agafa (n-1) xs

-- elimina els n primers elements d'una llista
deixa :: Int -> [a] -> [a]
deixa 0 xs = xs
deixa _ [] = []
deixa n (_:xs) = deixa (n-1) xs

-- conversio entre Int i TipusSeg
tipusSegAInt :: TipusSeg -> Int
tipusSegAInt PSH = 0
tipusSegAInt ACK = 1
tipusSegAInt SYN = 2
tipusSegAInt FIN = 3

intATipusSeg :: Int -> TipusSeg
intATipusSeg 0 = PSH
intATipusSeg 1 = ACK
intATipusSeg 2 = SYN
intATipusSeg 3 = FIN

-- converteix una llista de bits en un valor de tipus TCPSegment
-- els primers 24 bits corresponen a la capçalera:
-- 8 bits pel tipus de segment, 8 pel número de seqüència i 8 pel checksum
-- la resta de bits corresponen a les dades
bitsATCPSegment :: [Int] -> TCPSegment
bitsATCPSegment xs = TCPSegment (Capçalera tipus numSeq checkSum) (Dades dades)
  where
    -- separar en capçaleres i dades
    cap = agafa 24 xs
    dades = deixa 24 xs
    
    -- separar cada part de la cap
    bitsTipus = agafa 8 cap
    bitsNum = agafa 8 (deixa 8 cap)
    bitsCheck = deixa 16 cap

    -- el tipus ha de ser de TipusSeg
    tipus :: TipusSeg
    tipus = intATipusSeg (vuitBitaInt bitsTipus)
    numSeq = vuitBitaInt bitsNum
    checkSum = bitsCheck

-- converteix una llista de llistes de bits en una llista de valors de tipus TCPSegment
-- aplica la funcio bitsATCPSegment a cada element de la llista
-- xs : resultat acumulat
llistaTCPSegments :: [[Int]] -> [TCPSegment]
llistaTCPSegments = foldr (\x xs -> (bitsATCPSegment x) : xs) []

-- obtenir la llista de TCPSegments a partir dels segments rebuts
segmentsTCP :: [TCPSegment]
segmentsTCP = llistaTCPSegments segmentsRebuts

-- calcula el checksum d'una llista de dades
-- separa les dades en blocs de 8 bits i fa la suma bit a bit
calculaCheckSum :: Dades -> CheckSum
calculaCheckSum (Dades xs) = sumarBitaBit (parts8bits xs)

-- comprova si un segment te error a les dades
-- un segment es correcte si el checksum calculat coincideix amb el checksum guardat a la capçalera
teError :: TCPSegment -> Bool
teError (TCPSegment (Capçalera _ _ check) dades) = calculaCheckSum dades /= check

-- retorna nomes els segments sense error
-- si el segment x teErrors llavors la descarta i retorna la llista acumulada
segmentsSenseError :: [TCPSegment] -> [TCPSegment]
segmentsSenseError = foldr (\x xs -> if teError x then xs else x:xs) []

-- indica si un segment es de tipus PSH
esPSH :: TCPSegment -> Bool
esPSH (TCPSegment (Capçalera tipus _ _) _) = tipus == PSH

-- filtra nomes els segments de tipus PSH
filtraPSH :: [TCPSegment] -> [TCPSegment]
filtraPSH [] = []
filtraPSH (x:xs)
    | esPSH x = x : filtraPSH xs
    | otherwise = filtraPSH xs

-- retorna el numero de sequencia d'un segment
numSeqSegment :: TCPSegment -> NumSeq
numSeqSegment (TCPSegment (Capçalera _ numSeq _) _) = numSeq

-- indica si un segment pertanya a una llista
pertany :: TCPSegment -> [TCPSegment] -> Bool
pertany _ [] = False
pertany s (x:xs) = s == x || pertany s xs

-- elimina els segments duplicats d'una llista
treuDuplicats :: [TCPSegment] -> [TCPSegment]
treuDuplicats [] = []
treuDuplicats (x:xs)
    | pertany x xs = treuDuplicats xs
    | otherwise = x : treuDuplicats xs

-- insereix un segment en una llista ja ordenada segons el numero de sequencia
-- el mes petit al principi
insereixOrdenat :: TCPSegment -> [TCPSegment] -> [TCPSegment]
insereixOrdenat s [] = [s]
insereixOrdenat s (x:xs)
    | numSeqSegment s < numSeqSegment x = s : x : xs
    | otherwise = x : insereixOrdenat s xs

-- ordena una llista de segments segons el numero de sequencia
ordenaSegments :: [TCPSegment] -> [TCPSegment]
ordenaSegments [] = []
ordenaSegments (x:xs) = insereixOrdenat x (ordenaSegments xs)

-- filtra els segments PSH, elimina duplicats i els ordena
preparaSegments :: [TCPSegment] -> [TCPSegment]
preparaSegments xs = ordenaSegments . treuDuplicats . filtraPSH $ xs

-- extreu la llista de bits de les dades d'un segment
bitsDades :: TCPSegment -> [Int]
bitsDades (TCPSegment _ (Dades xs)) = xs

-- converteix una llista de bits en un String
-- separa la llista en blocs de 8 bits i transforma cada bloc en un caracter
bitsAString :: [Int] -> String
bitsAString xs = foldr (\x acc -> bit8Achar x : acc) [] (parts8bits xs)

-- obte el text d'un segment
textSegment :: TCPSegment -> String
textSegment seg = bitsAString (bitsDades seg)

-- concatena els textos de tots els segments
concatenaText :: [TCPSegment] -> String
concatenaText = foldr (\seg acc -> textSegment seg ++ acc) []

-- missatge final
missatgeFinal :: String
missatgeFinal = concatenaText . preparaSegments . segmentsSenseError $ segmentsTCP

---------------------------------------------------------------------------------------------------------------------
-------------------------------------------06lab---------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------

{-
Un analitzador per un tipus a es una funcio que te com a parametre
un String que es el contingut a analitzar. 
Si l'analisi:
 -  te exit la sortida es el valor analitzat juntament amb la resta de l'entrada,
 - si falla la sortida es Nothing.
-}


newtype Analitzador a = Analitzador { execAnalitzador :: String -> Maybe (a, String) }

instance Functor Analitzador where
    fmap f (Analitzador g) =  
      Analitzador (\s ->
            case g s of
                Nothing -> Nothing
                Just (x, resta) -> Just (f x, resta))
-- volem transformar el valor analitzat x en f x, pero sense tocar la resta del String de entrada

instance Applicative Analitzador where
    -- deixa la entrada igual en el pure
    pure x = Analitzador (\s -> Just (x, s))
    (Analitzador ffi) <*> (Analitzador fv) = 
      Analitzador (\s -> 
          case ffi s of
            Nothing -> Nothing
            Just (g, xs) ->
              case fv xs of
                Nothing -> Nothing
                Just (x, ys) -> Just (g x, ys))
{-
ffi, que ha de retornar una funcio
fv, que ha de retornar un valor

-}

instance Alternative Analitzador where
    empty = Analitzador (\_ -> Nothing)
    p1 <|> p2 = 
      Analitzador (\s ->
        case execAnalitzador p1 s of
          Nothing -> execAnalitzador p2 s
          Just (x,xs) -> Just (x,xs))
{-
  p1 <|> p2 prova primer p1 sobre la mateixa entrada s
  si p1 falla, llavors prova p2
  si p1 funciona, ja ens quedem aquell resultat
-}


{-
 'complirA' té com a parametre una funcio que representa un predicat
 sobre un Char i té com a resultat un analitzador. 
 Aquest analitzador: 
    - te exit nomes si veu un Char que compleix el predicat
      (que alhora es el Char retornat per l'analitzador). 
    - si troba un caracter que no compleix el predicat (o l'entrada
      es buida) llavors falla i la sortida es Nothing.
-}


complirA :: (Char -> Bool) -> Analitzador Char
complirA predicat = Analitzador f
  where
    f [] = Nothing    -- falla si l'entrada es buida
    f (x:xs)           
        -- Comprova si x compleix el predicat
        | predicat x       = Just (x, xs) -- si es que si
                                   -- retorna x i la resta de 
                                   -- l'entrada (es a dir, xs)

        | otherwise = Nothing  -- altrament, falla
{-
Aqui nomes mira:
x, que es el primer caracter
si compleix, el consumeix
si no compleix, falla directament
-}


{-
 Mijançant complirA, es pot definir un analitzador - caracterA crctr -
 que espera trobar exactament el caracter crctr i si no falla.
-}
caracterA :: Char -> Analitzador Char
caracterA c = complirA (== c)

{- per exemple:

> execAnalitzador (complirA isUpper) "ABC"
Just ('A',"BC")
> execAnalitzador (complirA isUpper) "abc"
Nothing
> execAnalitzador (caracterA 'x') "xyz"
Just ('x',"yz")

---------------- en realitat nomes comprova amb el primer char que es troba dins el string d'entrada -------------------------------
-}

{-
 Si l'entrada comença per caracters numerics els retorna tots
 fins que troba un caracter no numeric.

 Exemple:

 execAnalitzador enterPosA "234abc"
     Just (234,"abc")
-}
enterPosA :: Analitzador Int
enterPosA = Analitzador f
  where
    f xs
      | null enters   = Nothing
      | otherwise = Just (read enters, elQueQueda)
      where (enters, elQueQueda) = primerTram isDigit xs

{-
 Donada una llista la sortida es una llista amb
 els primers valor consecutius que compleixen el
 predicat i una llista amb la resta de valors


  ghci> primerTram (isDigit) "1234abc1234"
  ("1234","abc1234")

-}

primerTram :: (a -> Bool) -> [a] -> ([a], [a])
primerTram _ [] = ([],[])
primerTram predicat (x:xs)
                | predicat x = let (tmpP, tmpR) = primerTram predicat xs 
                               in (x:tmpP, tmpR)
                | otherwise = ([],x:xs)

{- Exemple:
data Producte = P {codi :: Char, quantitat :: Int}
    deriving Show

analitzarProducte :: Analitzador Producte
analitzarProducte = P <$> complirA isUpper <*> enterPosA


ghci> execAnalitzador analitzarProducte "B345"
Just (P {codi = 'B', quantitat = 345},"")

ghci> execAnalitzador analitzarProducte "345B"
Nothing
-}


{-
capOmes te com a entrada un analitzador i l'executa repetidament 
tantes vegades com sigui possible (que pot ser cap vegada, 
si falla d'entrada) i retorna una llista dels resultats.
capOmes sempre te exit.
-}
capOmes :: Analitzador a -> Analitzador [a]
capOmes p = 
  Analitzador (\s ->
    case execAnalitzador p s of
      Nothing -> Just ([], s)
      Just (x, xs) ->
        case execAnalitzador (capOmes p) xs of
          Just (ys, zs) -> Just (x:ys, zs))
{-
capOmes p sera un analitzador que rep un String s
intenta fer unaOmes p
si falla, llavors retorna la llista buida i deixa l'entrada igual
si funciona, retorna aquell resultat
-}


-- Exemples capOmes
eXcapOmes1 = execAnalitzador (capOmes enterPosA) "123abc"
-- Just ([123],"abc")
--OK

eXcapOmes2 = execAnalitzador (capOmes enterPosA) "abc123"
-- Just ([],"abc123")
--OK


{-
unaOmes te com a entrada un analitzador i l'executa repetidament 
tantes vegades com sigui possible, pero a diferencia de capOmes
requereix que l'analitzador d'entrada tingui exit almenys una vegada.
Si l'analitzador d'entrada falla la primera vegada llavors
unaOmes tambe falla.
-}
unaOmes :: Analitzador a -> Analitzador [a]
unaOmes p =  
  Analitzador (\s ->
    case execAnalitzador p s of
      Nothing -> Nothing
      Just (x, xs) ->
        case execAnalitzador (capOmes p) xs of
          Just (ys, zs) -> Just (x:ys, zs))
{-
prova p sobre l'entrada actual
si p falla a la primera, retorna Nothing
si p funciona, obte:
el primer valor x
la resta de l'entrada xs
despres aplica capOmes p sobre xs per llegir les repeticions restants
-}

-- Exemples unaOmes
eXunaOmes1 = execAnalitzador (unaOmes enterPosA) "123abc"
-- Just ([123],"abc")
--OK

eXunaOmes2 = execAnalitzador (unaOmes enterPosA) "abc123"
-- Nothing
--OK
-- Observar la diferencia amb capOmes

{- Observacions: 

- Per analitzar una o mes execucions d'un analitzador anltzdr, 
  executar anltzdr una vegada i despres analitzar cap o mes 
  vegades l'execucio d'anltzdr.

- Per analitzar cap o mes execucions d'un analitzador anltzdr,
  executar anltzdr una vegada o mes i si falla retornar la 
  llista buida.
-}


{-
espaisA analitza una llista consecutiva de cap o mes espais en blanc.
Per la seva implementacio es util fer servir:

  isSpace :: Char -> Bool

Exemples:

ghci> execAnalitzador espaisA "     abc"
    Just ("     ","abc")
OK

ghci> execAnalitzador espaisA "abc    " 
Just ("","abc    ")
OK
-}
espaisA :: Analitzador String
espaisA = capOmes (complirA isSpace)

{-
complirA isSpace analitza un espai
capOmes (complirA isSpace) analitza cap o mes espais
com que el resultat es una llista de Char, el tipus final es String
-}


{-
bitCharA analitza un caracter i te exit si es '0' o '1'.
Altrament falla.

Exemples:

ghci> execAnalitzador bitCharA "1"      
Just ('1',"")
OK

ghci> execAnalitzador bitCharA "10"
Just ('1',"0")
OK

ghci> execAnalitzador bitCharA " 1" 
Nothing
OK
-}

bitCharA :: Analitzador Char
bitCharA = 
  Analitzador (\s ->
    case execAnalitzador (caracterA '0') s of
      Nothing -> execAnalitzador (caracterA '1') s
      Just (x, xs) -> Just (x, xs))
{-
analitza un sol caracter
comprova si es '0'
si no, comprova si es '1'
si no es cap dels dos, falla
-}


{- 
 Igual que bitCharA pero en cas d'exit el 
 resultat de l'analitzador es un enter
-}
bitA :: Analitzador Int
bitA = digitToInt <$> bitCharA
{-
primer reconeix '0' o '1'
despres ho converteix a 0 o 1
-}

{-
 L'analitzador te exit si l'entrada es un String que
 comença per  un seguit de '0's i/o '1's 
-}
bitsA :: Analitzador [Int]
bitsA = unaOmes bitA
{-
usa bitA per llegir un bit
usa unaOmes per llegir una o mes vegades
per aixo retorna una llista [Int]
-}


coincideix :: String -> Analitzador String 
coincideix [] = pure []
coincideix (x:xs) = pure (:) <*> (caracterA x) <*> (coincideix xs)
-- coincideix (x:xs) = liftA2 (:) (caracterA x) (coincideix xs)

{-
execAnalitzador (coincideix "abc") "abc"
  Just ("abc","")
  OK

ghci> execAnalitzador (coincideix "abc") "abcd"  
Just ("abc","d")
OK

ghci> execAnalitzador (coincideix "abc") "bcd" 
Nothing
OK
-}

{-
(<$) :: a -> f b -> f a 
Replace all locations in the input with the same value. 
The default definition is fmap . const.

Exemple:

ghci> 'a' <$ (Just True)
Just 'a'

-}

tipusSegA :: Analitzador TipusSeg
tipusSegA = 
    PSH <$ coincideix "PSH"
    <|> ACK <$ coincideix "ACK"
    <|> SYN <$ coincideix "SYN"
    <|> FIN <$ coincideix "FIN"
{-
tipusSegA:
usa coincideix per reconeixer el text
usa <$ per canviar el String reconegut dins al constructor
va provant les quatre opcions possibles

<|>:
primer analitzador
si falla, segon analitzador
si el primer funciona, el segon ni es mira
-}


{-
Observacions:

 Per analitzar alguna 'cosa' i ignorar el seu resultat es poden fer servir 
 les funcions (*>) i (<*) :
     
    (*>) :: Applicative f => f a -> f b -> f b
    (<*) :: Applicative f => f a -> f b -> f a

  f1 *> f2 executa primer f1, llavors f2 pero ignora el resultat de f1,
  i dona com a resultat el resultat de f2.
 
  f1 <* f2 tambe executa primer f1, llavors f2 pero dona com a resultat 
  el resultat de f1 (i ignora el resultat de f2)
 
 Exemples:

    ghci> execAnalitzador (caracterA 'a' *> enterPosA ) "a123bc"
    Just (123,"bc")
    OK

    ghci> execAnalitzador (caracterA 'a' *> enterPosA ) "b123cd"
    Nothing
    OK

    ghci> execAnalitzador (caracterA 'a' *> enterPosA ) "123abc"
    Nothing
    OK

    ghci> execAnalitzador (caracterA 'a' <*  enterPosA ) "a123abc"
    Just ('a',"abc")
    OK

    ghci> execAnalitzador (caracterA 'a' <* enterPosA ) "b123cd"  
    Nothing
    OK
 
-}


{- 
   Funcio que donat un caracter te com a resultat un analitzador que
   'elimina' espais abans i despres del caracter a l'entrada

  Exemples:

    ghci> execAnalitzador (espCaracteResp  '*') "    *    "
    Just ('*',"")
    OK

  Pero observar que

    ghci> execAnalitzador (espCaracteResp  '*') "    *a    "
    Just ('*',"a    ")

-}
espCaracteResp :: Char -> Analitzador Char
espCaracteResp c = espaisA *> caracterA c <* espaisA
{-
primer consumeix espais que hi ha al principi
despres consumeix el caracter correceponent
torna a consumir espais i retorna amb caracter consumit i resultat final
-}



{-
 El format de les possibles capçaleres a analitzar es:

  espais '<' espais TipusSeg espais NumSeq espais CheckSum espais '>' espais

 L'analitzador capçaleraA té exit si es troba un string amb aquest format

 Exemple:

   capçaleraACK456 = "    <    ACK 456  01010101    >   "

   ghci> execAnalitzador capçaleraA capçaleraACK456 
   Just (Cap PSH 456 [0,1,0,1,0,1,0,1],"")
----------------------------  No hauria de ser ACK en lloc de PSH??  --------------------------------------------------------
   Just (Capçalera ACK 456 [0,1,0,1,0,1,0,1],"")
-}
capçaleraA :: Analitzador Capçalera
capçaleraA = 
  Capçalera
        <$> (espCaracteResp '<' *> tipusSegA <* espaisA)
        <*> (enterPosA <* espaisA)
        <*> (bitsA <* espCaracteResp '>')
{-
data Capçalera = Capçalera TipusSeg NumSeq CheckSum
consumeix espais, el simbol <, espais que hi hagi i treu el tipusSeg
llegeix l'enter, consumeix els espais que hi hagi despres
llegeix els bits, consumeix el caracter > amb espais si hi ha
-}
capçaleraACK456 = "    <    ACK 456  01010101    >   "


{-
 El format de les possibles dades a analitzar es:

  espais '{' espais 01···101 espais 01···101 espais ··· 01···101 espais '}' espais

 L'analitzador capçaleraA té exit si es troba un string amb aquest format

 Exemple:

   dades01 = " { 101010010101   01010101001    000 111 0 0 0 1 1 1  }  "

  ghci> execAnalitzador dadesA dades01
  Just (Dades [1,0,1,0,1,0,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,0,1,0,0,0,1,1,1,0,0,0,1,1,1],"")
  OK
-}

dadesA :: Analitzador Dades
dadesA = Dades . concat <$> (espCaracteResp '{' *> capOmes (espaisA *> bitsA) <* espCaracteResp '}')
{-
    espCaracteResp '{'
Analitza la clau { permetent espais abans i despres
    (espaisA *> bitsA)
consumeix els espais que hi hagi abans del grup
despres llegeix un grup de bits
    espCaracteResp '{' *> capOmes (espaisA *> bitsA) <* espCaracteResp '}'
despres de llegir tots els grups, consumeix la clau } del final

    Dades . concat (exemple):
concat [[1,0,1],[1,1],[0]] = [1,0,1,1,1,0]
Dades [1,0,1,1,1,0]
-}

dades01 = " { 101010010101   01010101001    000 111 0 0 0 1 1 1  }  "


{-
 Exemple:
 
 segmentPSH123 = "  <   PSH 123  01010101  >  {  011001100 110100 101100  001 }  "


 ghci> execAnalitzador segmentA segmentPSH123 
 Just (PSH - 123 - [0,1,0,1,0,1,0,1] - fia,"")
-------- Just (TCPSegment (Capçalera PSH 123 [0,1,0,1,0,1,0,1]) (Dades [0,1,1,0,0,1,1,0,0,1,1,0,1,0,0,1,0,1,1,0,0,0,0,1]),"") ???-----
-} 
segmentPSH123 = "  <   PSH 123  01010101  >  {  011001100 110100 101100  001 }  "

segmentA :: Analitzador TCPSegment
segmentA = TCPSegment <$> capçaleraA <*> dadesA


segmentsProcessats = " <  ACK  68    00000000     >     { }   <  PSH  144    00010110     >    {    01101110011000110111010001101111   }  <     ACK   82   00000000 >   {     }  <  PSH   136 01001101   > {  1 0 1 0000   10010010   10010  01010  1001100  }  <   SYN   120   00000000  > {  }    <    PSH  140   01000000 > {  0  11100  11001  00000  01100 110  0111 0101   }    <  PSH   120  00111111   > { 010000 10011 0010101    10111   0011   10110 }  <   ACK   144  00000000 >    {     }   <    PSH  140   01000000 > {  0  10100  11001  00000  01110 110  1111 0101   }    <   PSH 124  00010101  >     {   011   010010  1101110  01100111  01110101   } <FIN 234 00000000  >{     }   < PSH  128     01011001>   {    0  1110  10000100000011000010110110  0  }<PSH 132  01001100  > {  00100   000011   01101011   01111  011011  1 0  }  <  PSH   120  00111111   > { 010000 10011 0010101    10111   0011   10110 }   <  PSH   136 01001101   > {  0 0 1 0000   00110010   00110  01010  1101100  }  <FIN 234 00000000  >{     }     <     PSH  148    00100000  >  {  0111   00100  11100  1100100001  }"


segmentsA :: Analitzador [TCPSegment]
segmentsA = capOmes segmentA
{-
segmentA analitza un segment
capOmes segmentA analitza zero o mes segments seguits
i retorna la llista de tots ells
-}

{-
Just ([TCPSegment (Capçalera ACK 68 [0,0,0,0,0,0,0,0]) (Dades []),
TCPSegment (Capçalera PSH 144 [0,0,0,1,0,1,1,0]) (Dades [0,1,1,0,1,1,1,0,0,1,1,0,0,0,1,1,0,1,1,1,0,1,0,0,0,1,1,0,1,1,1,1]),
TCPSegment (Capçalera ACK 82 [0,0,0,0,0,0,0,0]) (Dades []),
TCPSegment (Capçalera PSH 136 [0,1,0,0,1,1,0,1]) (Dades [1,0,1,0,0,0,0,1,0,0,1,0,0,1,0,1,0,0,1,0,0,1,0,1,0,1,0,0,1,1,0,0]),
TCPSegment (Capçalera SYN 120 [0,0,0,0,0,0,0,0]) (Dades []),
TCPSegment (Capçalera PSH 140 [0,1,0,0,0,0,0,0]) (Dades [0,1,1,1,0,0,1,1,0,0,1,0,0,0,0,0,0,1,1,0,0,1,1,0,0,1,1,1,0,1,0,1]),
TCPSegment (Capçalera PSH 120 [0,0,1,1,1,1,1,1]) (Dades [0,1,0,0,0,0,1,0,0,1,1,0,0,1,0,1,0,1,1,0,1,1,1,0,0,1,1,1,0,1,1,0]),
TCPSegment (Capçalera ACK 144 [0,0,0,0,0,0,0,0]) (Dades []),
TCPSegment (Capçalera PSH 140 [0,1,0,0,0,0,0,0]) (Dades [0,1,0,1,0,0,1,1,0,0,1,0,0,0,0,0,0,1,1,1,0,1,1,0,1,1,1,1,0,1,0,1]),
TCPSegment (Capçalera PSH 124 [0,0,0,1,0,1,0,1]) (Dades [0,1,1,0,1,0,0,1,0,1,1,0,1,1,1,0,0,1,1,0,0,1,1,1,0,1,1,1,0,1,0,1]),
TCPSegment (Capçalera FIN 234 [0,0,0,0,0,0,0,0]) (Dades []),
TCPSegment (Capçalera PSH 128 [0,1,0,1,1,0,0,1]) (Dades [0,1,1,1,0,1,0,0,0,0,1,0,0,0,0,0,0,1,1,0,0,0,0,1,0,1,1,0,1,1,0,0]),
TCPSegment (Capçalera PSH 132 [0,1,0,0,1,1,0,0]) (Dades [0,0,1,0,0,0,0,0,0,1,1,0,1,1,0,1,0,1,1,0,1,1,1,1,0,1,1,0,1,1,1,0]),
TCPSegment (Capçalera PSH 120 [0,0,1,1,1,1,1,1]) (Dades [0,1,0,0,0,0,1,0,0,1,1,0,0,1,0,1,0,1,1,0,1,1,1,0,0,1,1,1,0,1,1,0]),
TCPSegment (Capçalera PSH 136 [0,1,0,0,1,1,0,1]) (Dades [0,0,1,0,0,0,0,0,0,1,1,0,0,1,0,0,0,1,1,0,0,1,0,1,0,1,1,0,1,1,0,0]),
TCPSegment (Capçalera FIN 234 [0,0,0,0,0,0,0,0]) (Dades []),
TCPSegment (Capçalera PSH 148 [0,0,1,0,0,0,0,0]) (Dades [0,1,1,1,0,0,1,0,0,1,1,1,0,0,1,1,0,0,1,0,0,0,0,1])],"")
-}

missatgeSegmentsProcessats :: Maybe String
missatgeSegmentsProcessats =
  case execAnalitzador segmentsA segmentsProcessats of
    Nothing -> Nothing
    Just (segs, _) -> Just (concatenaText (preparaSegments (segmentsSenseError segs)))