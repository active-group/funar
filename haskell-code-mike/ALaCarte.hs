{-# LANGUAGE TypeOperators #-}

module ALaCarte where

data SimpleArith e =
    SVal Int | SAdd e e

data Expr f = In (f (Expr f))

data Val e = Val Int -- Val :: * -> *

data Add e = Add e e -- Add :: * -> *

data Mul e = Mul e e

-- Produkt: zusammgesetzte Daten
-- kartesische Produkt - Tupel
-- X ist das Produkt von Int und String
-- Int x String, F# int * string
type X = (Int, String)

-- Coprodukt: gemischte Daten, "Summe" - |
-- data Either a b = Left a | Right b

data (f :+: g) e = -- f :: * -> *, g :: * -> *
    Inl (f e) -- "inject left"
  | Inr (g e) -- "inject right"


addExample :: Expr (Val :+: Add)
-- "118 + 1219"
addExample = In (Inr (Add (In (Inl (Val 118))) (In (Inl (Val 1219)))))

instance Functor Val where
    fmap f (Val x) = Val x 

instance Functor Add where 
    fmap f (Add e1 e2) = Add (f e1) (f e2)

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap function (Inl e1) = Inl (fmap function e1)
    fmap function (Inr e2) = Inr (fmap function e2)

foldExpr ::  Functor f => (f a -> a) -> Expr f -> a
foldExpr combine (In t) =
    combine (fmap (foldExpr combine) t)

class Functor f => Eval f where
    evalAlgebra :: f Int -> Int 

instance Eval Val where
    evalAlgebra (Val x) = x 

instance Eval Add where
    evalAlgebra (Add x y) = x + y

instance (Eval f, Eval g) => Eval (f :+: g) where
    evalAlgebra (Inl x) = evalAlgebra x 
    evalAlgebra (Inr y) = evalAlgebra y

eval :: Eval f => Expr f -> Int
eval expr = foldExpr evalAlgebra expr