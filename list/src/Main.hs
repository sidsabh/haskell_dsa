{-# LANGUAGE InstanceSigs #-}
module Main (main) where

data List a = Null | Cons a (List a) deriving Show

instance Functor List where
    fmap :: (a -> b) -> List a -> List b
    fmap f (Cons e le) = Cons (f e) (fmap f le)
    fmap _ Null = Null

append' :: List a -> List a -> List a
append' Null x2 = x2
append' x1 Null = x1
append' (Cons a Null) x2 = Cons a x2
append' (Cons a x1') x2 = Cons a (append' x1' x2)

instance Applicative List where
    pure e = Cons e Null
    (<*>) :: List (a -> b) -> List a -> List b
    (Cons f lf) <*> as@(Cons _ _) = 
        let first = fmap f as
            second = lf <*> as
        in append' first second
    _ <*> _ = Null


concat' :: List (List a) -> List a
concat' Null = Null
concat' (Cons l ll) = lprocess l ll where 
    lprocess :: List a -> List (List a) -> List a
    lprocess x Null = x
    lprocess Null xs = concat' xs
    lprocess (Cons a Null) xs = Cons a (concat' xs)
    lprocess (Cons a la) xs = Cons a (lprocess la xs)

instance Monad List where
    (>>=) :: List a -> (a -> List b) -> List b
    (>>=) ma f = 
        concat' $ fmap f ma


-- Cartesian product (product type)
pairs :: List a -> List b -> List (a, b)
pairs la lb = do 
    a <- la  -- to actually get the cartesian product
    b <- lb  -- to create a list of lambdas is just fmap, but
    return (a,b)

main :: IO ()
main = 
    let l1 = Cons 1 (Cons 2 (Cons 3 Null)) :: List Integer
        l2 = Cons 10 (Cons 20 Null) :: List Integer
        prod = pairs l1 l2
    in print prod 