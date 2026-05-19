{-
data Estat e a = Estat (e -> (a,e))
    -- li passo un estat, retorna un estat nou i un valor

insatance Functor (Estat e) where
    -- fmap :: (a -> b) -> Estat e a -> Estat e b
    fmap f (Estat g) = Estat $ \est -> let (v, est') = g est 
        in (f v, est')

instance Applicative (Estat e) where
    -- pure :: a -> Estat e a
    pure x = Estat $ \est -> -> (x, est)
    -- (<*>) :: Estat e (a -> b) -> Estat e a -> Estat e b
    -- est ->| ->| fg |-> (g, est1) <*> est1 ->| fv |-> (v2, est2) ->|-> (g v2, est2)
    -- fg = Estat e (a->b)
    -- fv = Estat e a
    Estat fg <*> Estat fv = Estat $ \est -> 
        let (g, est1)  = fg est
            (v2, est2) = fv est1
        in (g v2, est2)

instance Monad (Estat e) where
    return = pure
    (>>=) :: Estat e a -> (a -> Estat e b) -> Estat e b
    -- est ->| ->|fx| -> (x, est1) -> est1 -> |h| -> | -> (y, est2) 
    (Estat fx) >>= g = Estat $ \est -> 
        let (x, est1) = fx est
            Estat h = g x
        in h est1
-}