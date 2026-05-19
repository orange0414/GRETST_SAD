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