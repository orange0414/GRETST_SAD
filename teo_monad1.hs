{-
Notacio do

-   bind : t a >>= \a -> t b >>= \b -> t c >>= \c -> t d
            ------------------ :: t b
            -------------------------------- :: t c
            ---------------------------------------------- :: t d

    t a >>= \a ->
        t n >>= \b ->
                t c >>= \c ->
                        t d

    t a >>= \a ->
    t b >>= \b ->
    t c >>= \c ->
    t d

----------- t x >>= \x ... == do x <- t x --------------

- do 
    a <- t a
    b <- t b
    c <- t c
    t d

instance Monad Maybe where
    -- pure x = Just x
    return = pure
    -- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    Nothing >>= _ = Nothing
    (Just x) >>= f = f x

-}