module RecursionSchemes (Algebra, Coalgebra, hylo) where

type Algebra f x = f x -> x

type Coalgebra f x = x -> f x

newtype Fix f = In { out :: f(Fix f) }

cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . out

ana :: Functor f => Coalgebra f a -> a -> Fix f
ana coa = In . fmap (ana coa) . coa

hylo :: Functor f => Algebra f a -> Coalgebra f b -> b -> a
hylo alg coa = alg . fmap (hylo alg coa) . coa

