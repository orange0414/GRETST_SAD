import Data.Char -- 'toUpper'


-- fmap :: (a -> b) -> f a -> f b
-- una funcio normal a -> b
-- un valor dins d'un context f a
-- obtenim el resultat transformat dins del mateix context f b

{-
aqui f no es un valor, sino un constructor de tipus
(<$>) = fmap
fmap (*2) (Just 5)
(*2) <$> Just 5
-}

{-
instance Functor [] where
     fmap = map

en llistes fmap es simplement map
aplica la funcio a cada element, mantenint l'ordre i l'estructura de la llista
-}

{-
instance Functor (Either e) where
    fmap _ (Left err) = Left err
    fmap f (Right x)  = Right (f x)

Right x es el valor correcte -> s'hi aplica la funcio
Left err es l'error -> es deixa igual
-}

{-
instance Functor IO where
    fmap f action = do { x ← action ; return ( f x ) }

s'aplica sobre el resultat del action
-}

{-
instance Functor ((,) e) where
    fmap f (x, y) = (x, f y)

en una parella, el Functor actua sobre el segon component
fmap (*2) ("clau", 5)  -> ("clau", 10)
-}

{-
instance Functor ((->) r) where
    fmap f g = f . g

per a funcions, fmap es (.)
-}

{-
Llei de composició
fmap (f . g) = fmap f . fmap g

fmap (*2) (fmap (+1) (Just 3)) = Just 8
fmap ((*2) . (+1)) (Just 3)    = Just 8
-}

{-
data Arbre a = Buit | Node a (Arbre a) (Arbre a)
instance Functor Arbre where
    fmap _ Buit = Buit
    fmap f (Node x e d) = Node (f x) (fmap f e) (fmap f d)

data LlistaPr a = Buida | Cons a (LlistaPr a)
instance Functor LlistaPr where
    fmap _ Buida = Buida
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

data Etiquetat e a = Etiquetat e a
instance Functor (Etiquetat e) where
    fmap f (Etiquetat et x) = Etiquetat et (f x)
-- L'etiqueta e queda igual, nomes canvia el valor a

data F b a = F (b -> a)
instance Functor (F b) where
    fmap f (F g) = F (f . g)
-- 'F b' -> constructor
-}


--------------------------- 6. Exercicis bàsics --------------------------------------
-- Exercici 1 — Familiarització amb fmap ---------------------------------------------
-- (a)
ej1a = fmap (+10) ( Just 5) -- = Just (5+10) = Just 15 -> 2.2 elevar una funcio
-- (b)
ej1b = fmap (+10) Nothing -- com Just _ Nothing = Nothing -> dona Nothing -> 3.2 valor opcional
-- (c)
ej1c = fmap (*3) [1 , 2 , 3 , 4] -- = [1*3, 2*3, 3*3, 4*3] = [3, 6, 9, 12] -> 3.1 Llistes: fmap es map
-- (d)
ej1d = fmap show ( Just 42) -- = Just (show 42) = Just "42" -> 2.2 elevar una funcio
-- (e)
ej1e = fmap length ( Just " programacio ") -- Just (length " programacio ") = Just 13 -> les espais + lletres de la paraula
-- (f)
ej1f = fmap (++ "!") [" hola ", " adeu ", " bon dia "] 
-- [(++ "!") " hola ", (++ "!") " adeu ", (++ "!") " bon dia "] = [" hola !"," adeu !"," bon dia !"] -> 3.1 Llistes: fmap es map
-- (g)
ej1g = fmap (*2) ( Right 7 :: Either String Int ) -- = Right 14 -> es Right s'aplica 7*2 
-- Left String, Right Int
-- (h)
ej1h = fmap (*2) ( Left " error " :: Either String Int ) -- = Left " error " -> es Left no s'aplica (*2)
-- (i)
ej1i = fmap fst ( Just (3 , True ) ) -- = Just (fst (3, True)) = Just 3
-- fst : agafa el primer element d'una parella
-- (j)
ej1j = fmap ( map (*2) ) ( Just [1 ,2 ,3]) -- = Just ( map (*2) [1 ,2 ,3]) = Just [(*2) 1, (*2) 2 , (*2) 3] = Just [2, 4, 6]


-- Exercici 2 — fmap versus <$> -----------------------------------------------
-- (a) Reescriviu amb <$> :
ej2a = fmap ( map toUpper ) ( Just " hola ")
ej2a' = ( map toUpper ) <$> ( Just " hola ")
-- (b) Reescriviu amb <$> :
ej2b = fmap (*2) [10 , 20 , 30]
ej2b' = (*2) <$> [10 , 20 , 30]
-- (c) Reescriviu amb fmap :
ej2c = show <$> Just 3.14
ej2c' = fmap show (Just 3.14)
-- (d) Reescriviu amb fmap :
ej2d = (+1) <$> Right 99
ej2d' = fmap (+1) (Right 99)

-- Exercici 3 — Composició de fmap ------------------------------------------------
-- (a)
ej3a = fmap (*2) (fmap (+3) (Just 10))
ej3a' = fmap ((*2) . (+3)) (Just 10)
-- (b)
ej3b = fmap reverse (fmap words (Just " hola mon que tal "))
ej3b' = fmap (reverse . words) (Just " hola mon que tal ")
-- (c)
ej3c = fmap (map toUpper) (fmap (filter isAlpha) (Just "hola , mon !"))
ej3c' = fmap ((map toUpper) . (filter isAlpha)) (Just "hola , mon !")
-- (d)
ej3d = fmap show (fmap length [" hola ", " adeu "])
ej3d' = fmap (show . length) [" hola ", " adeu "]

{-
Exercici 4 — Verificació formal per a Maybe ---------------------------------------------
-- Llei 1 ( Identitat ): fmap id x = x
-- Cas x = Nothing :
fmap id Nothing
= Nothing -- per definicio de fmap
= Nothing -- resultat
-- Cas x = Just v:
fmap id ( Just v )
= Just (id v) -- per definicio de fmap
= Just v -- per definicio d’id
= Just v -- resultat
-- Llei 2 ( Composicio ): fmap (f . g) x = fmap f ( fmap g x)
-- Cas x = Nothing ( esquerra ):
fmap ( f . g ) Nothing = Nothing
-- Cas x = Nothing ( dreta ):
fmap f ( fmap g Nothing ) = fmap f Nothing = Nothing
-- Cas x = Just v ( esquerra ):
fmap ( f . g ) ( Just v ) = Just ((f . g) v)
-- Cas x = Just v ( dreta ):
fmap f ( fmap g ( Just v ) ) = fmap f (Just (g v)) = Just (f (g v))
-}

{-
-- Exercici 5 — Instàncies incorrectes -------------------------------------------------
-- (a)
data Caixa a = Caixa a deriving Show
instance Functor Caixa where
fmap _ ( Caixa x ) = Caixa x -- ignora la funcio
-- es viola la composicio, esta ignorant la funcio, en un cas correcte hauria d'aplicar la funico sobre el contingut
fmap f (Caixa x) = Caixa (f x)

-- (b)
data Parella a = Parella a a deriving Show
instance Functor Parella where
fmap f ( Parella x _ ) = Parella ( f x ) ( f x )
-- es viola la identitat, fmap id (Parella 3 5) = Parella 3 5, pero amb el donat per enunciat dóna Parella 3 3
fmap f (Parella x y) = Parella (f x) (f y)

-- (c)
instance Functor Maybe where
fmap _ _ = Nothing
-- Per a cada cas :
-- 1. Dieu quina llei es viola ( identitat , composicio , o totes dues ).
    viola identitat, perque toto ho converteix en Nothing
    no es veu clarament que viola la composicio, pero tambe podria ser ja que no diferencia quina funcio, tot acaba amb Nothing
-- 2. Doneu un contraexemple explicit .
    fmap id (Just 7) = Nothing /= Just 7
-- 3. Escriviu la instancia correcta .
    fmap _ Nothing  = Nothing
    fmap f (Just x) = Just (f x)
-}


-- Exercici 6 — Arbre rosa -------------------------------------------------------
data Rosa a = Fulla a | Branques [Rosa a]
    deriving Show
instance Functor Rosa where
    fmap f (Fulla x) = Fulla (f x)
    fmap f (Branques xs) = Branques (map (fmap f) xs)
-- Comproveu amb :
-- fmap (*2) ( Fulla 5)
-- fmap (*2) ( Branques [ Fulla 1 , Fulla 2 , Branques [ Fulla 3]])

-- Exercici 7 — Expressió aritmètica ---------------------------------------------
data Expr a
    = Literal a
    | Suma (Expr a) (Expr a)
    | Producte (Expr a) (Expr a)
    deriving Show

instance Functor Expr where
    fmap f (Literal x) = Literal (f x)
    fmap f (Suma e1 e2) = Suma (fmap f e1) (fmap f e2)
    fmap f (Producte e1 e2) = Producte (fmap f e1) (fmap f e2)

evalua :: Num a => Expr a -> a
evalua (Literal x) = x
evalua (Suma e1 e2) = evalua e1 + evalua e2
evalua (Producte e1 e2) = evalua e1 * evalua e2

expr :: Expr Int
expr = Suma (Literal 3) (Producte (Literal 4) (Literal 5))

-- (a)
-- evalua expr = 23

-- (b)
dobleLiterals :: Num a => Expr a -> Expr a
dobleLiterals = fmap (*2)

-- No, no es cert en general que evalua (dobleLiterals e) = 2 * evalua e
-- Contraexemple:
-- e = Producte (Literal 3) (Literal 4)
-- evalua (dobleLiterals e) = evalua (Producte (Literal 6) (Literal 8)) = 48
-- 2 * evalua e = 2 * 12 = 24

-- (c)
exprD :: Expr Double
exprD = fmap fromIntegral expr

-- evalua exprD = 23.0

-- Exercici 8 — Avalueu les expressions i comproveu els resultats al GHCi --------------------------
data F b a = F { runF :: b -> a }

instance Functor (F b) where
    fmap f (F g) = F (f . g)

doble :: F Int Int
doble = F (*2)

longitud :: F String Int
longitud = F length

-- (a)
ej8a = fmap show doble
-- tipus: F Int String
-- runF ej8a 5 = "10"

-- (b)
ej8b = fmap (*3) doble
-- tipus: F Int Int
-- runF ej8b 4 = 24

-- (c)
ej8c = fmap ((++"!") . show) longitud -- tipus: F String String
-- runF ej8c "hola" = "4!"

-- (d)
ej8d = F (show . (*2))
-- runF ej8d 5 = "10"

-- Exercici 9 — Encadenament de fmap --------------------------------------------
base :: F Int Int
base = F (+10)

-- (a)
pas1 :: F Int Int
pas1 = fmap (*2) base

resultat :: F Int String
resultat = fmap show pas1

resultatSimple :: F Int String
resultatSimple = fmap (show . (*2)) base

-- Comprovacio:
-- runF resultat 5 == runF resultatSimple 5
-- "30"


-- (b)
ej9b_enunciat = fmap length ( fmap words ( F (++ " mon ") ) )
ej9b = fmap (length . words) (F (++ " mon"))
-- aplicat a "hola":
-- runF ej9b "hola" = 2


-- (c)
f1 :: F Bool Int
f1 = F (\b -> if b then 1 else 0)

ej9c :: F Bool String
ej9c = fmap (\x -> if x == 1 then "si" else "no") f1

{- Exercici 10 — Verificacio de les lleis per a F b ---------------------------------------------------------
-- Recordeu: fmap f (F g) = F (f . g)

-- Llei 1 (Identitat): per a tot (F g),
-- fmap id (F g) = F g

fmap id (F g)
    = F (id . g)      -- per definicio de fmap
    = F g             -- per la propietat de id i composicio
    = F g             -- resultat


-- Llei 2 (Composicio): per a tot (F g),
-- fmap (f . h) (F g) = fmap f (fmap h (F g))

-- Costat esquerre:
fmap (f . h) (F g)
    = F ((f . h) . g)

-- Costat dret:
fmap f (fmap h (F g))
    = fmap f (F (h . g))      -- expandiu fmap h
    = F (f . (h . g))         -- expandiu fmap f
    = F ((f . h) . g)         -- associativitat de composicio
-}

-- Exercici 11 — Functor de funcions generalitzat ---------------------------------------------
-- (a)
toF :: (b -> a) -> F b a
toF g = F g

fromF :: F b a -> (b -> a)
fromF (F g) = g

-- Comproveu:
-- fromF . toF = id
-- fromF (toF g) = fromF (F g) = g = id g
--
-- toF . fromF = id
-- toF (fromF (F g)) = toF g = F g = id (F g)


-- (b)
-- fmap f (toF g)
-- = fmap f (F g)
-- = F (f . g)
-- = toF (f . g)
-- = toF (fmap f g)     -- on aquest fmap es el de ((->) b)
--
-- Aixo diu que la instancia Functor (F b) es exactament la mateixa
-- que la instancia estandard Functor ((->) b), nomes embolcallada amb F.


-- (c)
data G a b = G (a -> b)

instance Functor (G a) where
    fmap f (G g) = G (f . g)


-- (d)
liftF :: (F b a -> F b a) -> (b -> a) -> (b -> a)
liftF tr g = fromF (tr (toF g))

-- Explicacio:
-- liftF agafa una transformacio sobre F b a -> F b a
-- i la converteix en una transformacio equivalent sobre funcions b -> a.

-- Exemple d’us:
-- incF :: F Int Int -> F Int Int
-- incF = fmap (+1)
--
-- inc :: Int -> Int
-- inc = liftF incF
--
-- inc x = x + 1

-- Exercici 12 — Composicio de Functors amb F b --------------------------------------------------------

-- (a)
ex1 :: F Int (Maybe Int)
ex1 = F (\n -> if n > 0 then Just n else Nothing)

ex1' :: F Int (Maybe Int)
ex1' = fmap (fmap (*2)) ex1

-- runF ex1' 3    = Just 6
-- runF ex1' (-1) = Nothing


-- (b)
ex2 :: F String [Int]
ex2 = F (\s -> map length (words s))

ex2' :: F String [String]
ex2' = fmap (map show) ex2

-- runF ex2' "hola mon" = ["4","3"]


-- (c)
fmapDins :: Functor g => (a -> b) -> F r (g a) -> F r (g b)
fmapDins f = fmap (fmap f)

-- Usant fmapDins per a (b):
ex2'' :: F String [String]
ex2'' = fmapDins show ex2

-- Exercici 13 — Familiarització amb pure i <*> -------------------------------------------

-- (a)
ej13a = pure (+1) <*> Just 10
-- Just 11

-- (b)
ej13b = pure (+1) <*> Nothing
-- Nothing

-- (c)
ej13c = pure (*) <*> Just 3 <*> Just 4
-- Just 12

-- (d)
ej13d = (*) <$> Just 3 <*> Just 4
-- Just 12

-- (e)
ej13e = (++) <$> Just "hola" <*> Just " mon"
-- Just "hola mon"

-- (f)
ej13f = (++) <$> Nothing <*> Just " mon"
-- Nothing

-- Exercici 14 — Applicative amb llistes ----------------------------------------

-- (a)
ej14a = (+) <$> [1, 2, 3] <*> [1, 20]
-- [2,3,21,22,4,23]

-- Si voleu exactament:
ej14a' = (+) <$> [1, 2, 3] <*> [1, 2, 3]
-- [2,3,4,3,4,5,4,5,6]

-- Per obtenir [2,3,4,20,30,40]:
ej14a'' = (*) <$> [1,2,3] <*> [2,10]
-- [2,10,4,20,6,30]


-- (b)
ej14b = (,) <$> ['a','b','c'] <*> [1,2]
-- [('a',1),('a',2),('b',1),('b',2),('c',1),('c',2)]


-- (c)
ej14c = [succ, pred, (*2)] <*> [5,10]
-- [6,11,4,9,10,20]

-- Exercici 15 — Funcions amb multiples arguments --------------------------------------------

-- (a)
tripleProducte :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int
tripleProducte mx my mz = (\x y z -> x * y * z) <$> mx <*> my <*> mz

-- (b)
juntaParaules :: Maybe String -> Maybe String -> Maybe String
juntaParaules ms1 ms2 = (\x y -> x ++ " " ++ y) <$> ms1 <*> ms2

-- (c)
applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe mf mx = mf <*> mx

-- Exercici 16 — Els operadors *> i <* ----------------------------------------
-- <* conserva el resultat ESQUERRE , descarta el DRET
-- * > conserva el resultat DRET , descarta l’ESQUERRE
ej16a = Just 5 <* Just 10
-- Just 5
{-
-- const x y = x
Just 5 <* Just 10
= pure const <*> Just 5 <*> Just 10
= Just const <*> Just 5 <*> Just 10
= Just (const 5) <*> Just 10
= Just 5
-}

ej16b = Just 5 *> Just 10
-- Just 10
{-
Just 5 *> Just 10
= pure (\x y -> y) <*> Just 5 <*> Just 10
= Just (\x y -> y) <*> Just 5 <*> Just 10
= Just (\y -> y) <*> Just 10
= Just 10
-}

ej16c = Nothing <* Just 10
-- Nothing

ej16d = Just 5 *> Nothing
-- Nothing

ej16d' = Just 5 <* Nothing
-- Nothing
-- en el fons les dues <* i *> estan construits per <*> i  sabem que pel Maybe: 
-- _ <*> Nohting = Nothing
{-
Just 5 *> Nothing
= pure (\x y -> y) <*> Just 5 <*> Nothing
= Just (\x y -> y) <*> Just 5 <*> Nothing
= Just (\y -> y) <*> Nothing
= Nothing
-}

-- Exercici 17 — Validació de formularis ----------------------------------------------
data Usuari = Usuari
    { nom   :: String
    , edat  :: Int
    , email :: String
    } deriving (Show)

construirUsuari :: Maybe String -> Maybe Int -> Maybe String -> Maybe Usuari
construirUsuari mNom mEdat mEmail = Usuari <$> mNom <*> mEdat <*> mEmail
{-
Usuari <$> Just "Anna" <*> Just 25 <*> Just "anna@ex.com"
= Just (Usuari "Anna") <*> Just 25 <*> Just "anna@ex.com"
= Just (Usuari "Anna" 25) <*> Just "anna@ex.com"
= Just (Usuari "Anna" 25 "anna@ex.com")
-}

-- Exercici 18 — Producte cartesià generalitzat ----------------------------------------

-- (a)
combinacions :: [(Int, Char, Bool)]
combinacions = (,,) <$> [1,2] <*> ['a','b'] <*> [True, False]
-- [(1,'a',True),(1,'a',False),(1,'b',True),(1,'b',False),(2,'a',True),(2,'a',False),(2,'b',True),(2,'b',False)]

-- (b)
paraules2 :: [String]
paraules2 = (:) <$> ['a','b','c'] <*> ((:[]) <$> ['a','b','c'])
-- ["aa","ab","ac","ba","bb","bc","ca","cb","cc"]

-- (c)
tauler' :: [(Int, Int, Int)]
tauler' = (\x y -> (x, y, x*y)) <$> [1..4] <*> [1..4]
-- [(1,1,1),(1,2,2),(1,3,3),(1,4,4),(2,1,2),(2,2,4),(2,3,6),(2,4,8),(3,1,3),(3,2,6),(3,3,9),(3,4,12),(4,1,4),(4,2,8),(4,3,12),(4,4,16)]

-- Exercici 19 — Either per a validació amb errors -------------------------------
type Validat a = Either String a

validarNom :: String -> Validat String
validarNom s
    | length s < 2  = Left "El nom es massa curt"
    | length s > 50 = Left "El nom es massa llarg"
    | otherwise     = Right s

validarEdat :: Int -> Validat Int
validarEdat n
    | n < 0     = Left "L'edat no pot ser negativa"
    | n > 120   = Left "L'edat no es valida"
    | otherwise = Right n

data Persona = Persona String Int deriving Show

validarPersona :: String -> Int -> Validat Persona
validarPersona s n = Persona <$> validarNom s <*> validarEdat n

ej19a = validarPersona "Anna" 25
-- Right (Persona "Anna" 25)

ej19b = validarPersona "A" 25
-- Left "El nom es massa curt"

ej19c = validarPersona "Anna" (-5)
-- Left "L'edat no pot ser negativa"

ej19d = validarPersona "" (-5)
-- Left "El nom es massa curt"
-- hi ha mes d’un error, pero amb Either String nomes es veu el primer error que apareix d’esquerra a dreta