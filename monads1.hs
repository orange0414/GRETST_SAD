import Data.Char


--------------------------------------------------------------------------
--------------------------------------------------------------------------
------------       Monad amb nou valor (Maybe)      ----------------------
--------------------------------------------------------------------------
--------------------------------------------------------------------------

{- Que passa si per exemple volem limitar la longitud dels strings
   o no tractar els que  comencin per un caracter numeric, en aquest cas
   podem fer servir Maybe.
-}

-------------------------------------------------------------------------------
---- Funcions que retornen accions monadiques
-------------------------------------------------------------------------------

-- Nothing si la longitud de l'String supera un llindar
limit :: Int -> String -> Maybe String
limit l str
    |length str > l = Nothing
    |otherwise = Just str

-- Nothing si el primer caracter es numeric
noNumIni :: String -> Maybe String
noNumIni (x:xs)
    |isNumber x = Nothing
    |otherwise = Just (x:xs)

-- Converteix a tot majuscules
majs :: String -> Maybe String
-- majs str = Just (map toUpper str)
majs str = Just $ toUpper <$> str

-- Converteix a tot minuscules
mins :: String -> Maybe String
mins str = Just $ toLower <$> str

-- Posa primera lletra a majuscula
majInici :: String -> Maybe String
-- majInici (x:xs) = Just (toUpper x : xs)
majInici str = Just $ (toUpper (head str)):(tail str)

-- Posa caracter al final
carFin :: Char -> String -> Maybe String
carFin c str = Just $ str ++ [c]

-- Fent servir notacio bind (>>=) fer una funcio que 
-- donat un string el converteixi a un string en
-- minuscules pero que comença en majuscula i acaba en un '.'.
formatejarBindMb1 :: String -> Maybe String
formatejarBindMb1 str = (pure str) >>= mins >>= majInici >>= carFin '.'


-- Fent servir notacio do fer una funcio que 
-- donat un string el converteixi a un string 
-- en minuscules pero que comença per majuscula i acaba en un '.'.
formatejarDoMb1 :: String -> Maybe String
formatejarDoMb1 str = do 
    strMin <- mins str
    mi <- majInici strMin
    carFin '.' mi



-- Fent servir notacio bind (>>=) fer una funcio que 
-- donat un string el converteixi a un string en
-- minuscules que comença per majuscula i acaba en un '.',
-- o Nothing si comença amb un numero o supera els 10 caracters
formatejarBindMb2 :: String -> Maybe String
formatejarBindMb2 str = undefined

-- Fent servir notacio do fer una funcio que 
-- donat un string el converteixi a un string en
-- minuscules que comença per majuscula i acaba en un '.',
-- o Nothing si comença amb un numero o supera els 10 caracters
formatejarDoMb2 :: String -> Maybe String
formatejarDoMb2 str = undefined

-- Exemples:

exFormatBindR1 = formatejarBindMb1 "cervAntes"

exFormatDoR1 = formatejarDoMb1 "cervAntes" 

exFormatBindR2 = formatejarBindMb2 "1cervAntes"

exFormatDoR2 = formatejarDoMb2 "cervAntes         " 


{-
data Maybe a = Just a | Nothing

instance Functor Maybe where
    fmap _ Nothing       = Nothing 
    fmap f (Just x) = Just (f x)

-- (<$>) :: Functor f => (a->b) -> f a -> f b
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
instance Applicative Maybe where  
    pure = Just
    Nothing <*> _       = Nothing
    (Just f) <*> cx = f <$> cx

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
instance Monad Maybe where  
    return = pure
    Nothing >>= _ = Nothing
    Just x >>= f  = f x 
-}

--------------------------------------------------------------------------
--------------------------------------------------------------------------
------------              Monad Reader              ----------------------
--------------------------------------------------------------------------
--------------------------------------------------------------------------
{-
Suposem que tenim una capsa que per obrir-la hi hem de posar una fitxa, i 
el que hi ha dins la capsa quan s'obre depen de la fitxa que hi hem posat
-}

data CapsaF a b = Cf (a -> b)

produirValorCf :: CapsaF a b -> a -> b
produirValorCf (Cf g) ftx = g ftx

-- per treure de la capsa es necessita una fitxa.
fitxaCf :: CapsaF a a
fitxaCf = Cf (\ftx -> ftx)


-------------------------------------------------------------------------------
---- Fitxa (configuracio) 
-------------------------------------------------------------------------------

data FitxaStrings = FitxaS {majMin :: String, majIni :: Bool, carFi :: Char}
    deriving Show


-- Exemples:

conf :: FitxaStrings
conf = FitxaS {majMin = "min" , majIni = True, carFi = '.'}

recuperarFitxa :: FitxaStrings
recuperarFitxa = produirValorCf fitxaCf conf


-------------------------------------------------------------------------------
---- Funcions que retornen accions monadiques
-------------------------------------------------------------------------------

-- donat un string retorna una capsa, la qual si se li passa una fitxa, produira
-- l'string original converteix a majuscules o minuscules depenen de la fitxa
majMinCf :: String -> CapsaF FitxaStrings String
majMinCf str = do
    ftx <- fitxaCf
    if majMin ftx == "min" then pure $ fmap toLower str
    else pure $ fmap toUpper str


-- donat un string retorna una capsa, la qual si se li passa una fitxa, produira
-- l'string original amb la primera lletra a majuscula o minuscula 
-- depenen de la fitxa 
majIniciCf :: String -> CapsaF FitxaStrings String
majIniciCf str = do
    ftx <- fitxaCf
    if majIni ftx then pure $ (toUpper (head str)) : (tail str)
    else pure $ (toLower (head str)) : (tail str)


-- donat un string retorna una capsa, la qual si se li passa una fitxa, produira
-- l'string original amb el caracter que hi ha a la fitxa al final 
carFinCf :: String -> CapsaF FitxaStrings String
carFinCf str = do
    ftx <- fitxaCf
    pure $ str ++ [carFi ftx]

-- fent servir notacio bind (>>=) fer una funcio que donat un string 
-- retorna una capsa la qual, si se li passa una fitxa, produira
-- l'string original convertit a  maj/min, amb la primera lletra maj/min
-- i un caracter al final DEPENEN de la configuració que hi hagi a la fitxa.
formatejarBindCf :: String -> CapsaF FitxaStrings String
formatejarBindCf str = undefined


-- fent servir notacio do fer una funcio que donat un string 
-- retorna una capsa la qual, si se li passa una fitxa, produira
-- l'string original convertit a  maj/min, amb la primera lletra maj/min
-- i un caracter al final DEPENEN de la configuració que hi hagi a la fitxa.
formatejarDoCf :: String -> CapsaF FitxaStrings String
formatejarDoCf str = do
    str1 <- majMinCf str
    str2 <- majIniciCf str1
    carFinCf str2

-- Exemples:

conf1 :: FitxaStrings
conf1 = FitxaS {majMin = "min" , majIni = True, carFi = '.'}

conf2 :: FitxaStrings
conf2 = FitxaS {majMin = "maj" , majIni = True, carFi = ' '}


exFormatCf1 = produirValorCf (formatejarBindCf "cervAntes") conf1

exFormatCf2 = produirValorCf (formatejarBindCf "cervAntes") conf2

exFormatCf3 = produirValorCf (formatejarDoCf "cervAntes") conf1

exFormatCf4 = produirValorCf (formatejarDoCf "cervAntes") conf2




-- CapsaF functor
-- fmap :: (b -> c) -> CapsaF a b -> CapsaF a c
instance Functor (CapsaF a) where  
    fmap f (Cf t) = Cf (f . t)


-- CapsaF  Applicative
-- (<*>) :: CapsaF a (b -> c) -> CapsaF a b -> CapsaF a c
instance Applicative (CapsaF a) where  
    pure elem = Cf (\_ -> elem) 
    (Cf ff) <*> (Cf fx) = Cf (\ftx -> ff ftx (fx ftx))


-- CapsaF  Monad
-- (>>=) :: m b -> (b -> m c) -> m c
-- (>>=) :: CapsaF a b -> (b -> CapsaF a c) -> CapsaF a c
instance Monad (CapsaF a) where  
    return  = pure 
    Cf ft >>= f  = Cf (\ftx -> produirValorCf ( f (ft ftx)) ftx)

--------------------------------------------------------------------------
--------------------------------------------------------------------------
------------          Monad Reader (pre Analitzador)          ------------
--------------------------------------------------------------------------
--------------------------------------------------------------------------

{- 
I si volem que el que hi ha dins la capsa depengui d'una fitxa i a més
la capsa pugui estar buida?
-}

data CapsaFmb a b = Cfmb {treureCfMb :: a -> Maybe b}

fitxaMb :: CapsaFmb a a
fitxaMb = Cfmb(\ftx -> Just ftx)


------------------------------------------------------------------------------------
-- Fitxa 
-------------------------------------------------------------------------------------
data FitxaMb = FitxaCmb {limitCharMb :: Int, 
                         majMinMb :: String, 
                         majIniMb :: Bool, 
                         carFiMb :: Char, 
                         noNumIniMb :: Bool} -- no permet numero com a primer car

-------------------------------------------------------------------------------
---- Funcions que donen accions monadiques
-------------------------------------------------------------------------------

-- donat un string retorna una capsa, la qual si se li passa una fitxa, produira
-- l'string original o res si el numero de caracters de l'string es mes gran que 
-- l'especificat a la fitxa.
limitCfMb :: String -> CapsaFmb FitxaMb String
limitCfMb str = undefined


-- donat un string retorna una capsa, la qual si se li passa una fitxa, produira
-- l'string original o res si l'string comença amb un numero.
noNumIniCfMb :: String -> CapsaFmb FitxaMb String
noNumIniCfMb str = undefined

-- donat un string retorna una capsa, la qual si se li passa una fitxa, produira
-- l'string original amb la primera lletra a majuscula o minuscula 
-- depenen de la fitxa 
majMinCfMb :: String -> CapsaFmb FitxaMb String
majMinCfMb str = undefined

-- donat un string retorna una capsa, la qual si se li passa una fitxa, produira
-- l'string original amb la primera lletra a majuscula o minuscula 
-- depenen de la fitxa 
majIniciCfMb :: String -> CapsaFmb FitxaMb String
majIniciCfMb str = undefined


-- donat un string retorna una capsa, la qual si se li passa una fitxa, produira
-- l'string original amb el caracter que hi ha a la fitxa al final
carFinCfMb :: String -> CapsaFmb FitxaMb String
carFinCfMb str = undefined


-- fent servir notacio do fer una funcio que donat un string 
-- retorna una capsa la qual, si se li passa una fitxa, produira
-- l'string original convertit a  maj/min, amb la primera lletra maj/min
-- i un caracter al final DEPENEN de la configuració que hi hagi a la fitxa.
-- La capsa produira Nothing si el numero de caracters de l'string es mes 
-- gran que l'especificat a la fitxa. També produirà Nothing si l'string
-- comença amb un numero.
formatejarCfmb :: String -> CapsaFmb FitxaMb String
formatejarCfmb str = undefined


-- Exemples

confMb1 :: FitxaMb
confMb1 = FitxaCmb {limitCharMb = 10, majMinMb = "min", 
                    majIniMb = True, carFiMb = '.', noNumIniMb  = True}

confMb2 :: FitxaMb
confMb2 = FitxaCmb {limitCharMb = 6, majMinMb = "maj",
                    majIniMb = True, carFiMb = ' ', noNumIniMb  = True}

confMb3 :: FitxaMb
confMb3 = FitxaCmb {limitCharMb = 15, majMinMb = "maj",
                    majIniMb = True, carFiMb = ' ', noNumIniMb = True}


exFormatCfmb1 = treureCfMb (formatejarCfmb "cervAntes") confMb1

exFormatCfmb2 = treureCfMb (formatejarCfmb "cervAntes") confMb2

exFormatCfmb3 = treureCfMb (formatejarCfmb "cervAntes") confMb3

exFormatCfmb4 = treureCfMb (formatejarCfmb "1cervAntes") confMb3




-- fmap :: (b -> c) -> CapsaFmb a b -> CapsaFmb a c
instance Functor (CapsaFmb a) where  
    fmap f (Cfmb g) = Cfmb h
            where h = (f <$>) . g

-- CapsaFmb  Applicative
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
instance Applicative (CapsaFmb a) where  
    pure elem = Cfmb (\_ -> Just elem) 

    (Cfmb fmbfbc) <*> (Cfmb fmbb) = Cfmb (\ftx ->  (fmbfbc ftx) <*> (fmbb ftx))


-- CapsaFmb  Monad
-- (>>=) :: m b -> (b -> m c) -> m c
instance Monad (CapsaFmb a) where  
    return  = pure

    Cfmb fmba >>= f  = Cfmb $ \ftx -> case (fmba ftx) of 
                                            Nothing -> Nothing
                                            Just b -> (treureCfMb (f b)) ftx