{-# Language DeriveFunctor #-}

module RTree (fromSeed, fromRTree) where

import RecursionSchemes

data RTree a r = RT a [r]
    deriving (Functor)

fromSeed :: (v -> [v]) -> Coalgebra (RTree v) v 
fromSeed nxt p = RT p (nxt p)

fromRTree :: Algebra (RTree v) [v]
fromRTree (RT pr ps) = pr : concat ps
