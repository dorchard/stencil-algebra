{-# LANGUAGE TypeFamilies, FlexibleInstances, NoMonomorphismRestriction #-}


import GHC.Arr

{- All containers must be containers for stencil computations -}

import Control.Comonad

-- Some alises for niceness

current :: Comonad g => g a -> a
current = extract

promote :: Comonad g => (g a -> b) -> g a -> g b
promote = extend

{- All containers must have a neighbourhood coalgebra -}

class Neighbourhood g where
    type Marker g 
    neighbourhood :: g a -> [(g a, Marker g)]

neighbours :: (Neighbourhood g, Comonad g) => g a -> [a]
neighbours = (fmap (extract . fst)) . neighbourhood

{- <grid> -}

data Grid i a = Grid (Array i a) i deriving Show

instance Ix i => Functor (Grid i) where
    fmap f (Grid a i) = Grid (amap f a) i

instance Ix i => Comonad (Grid i) where
    extract (Grid a c) = a ! c
    extend k (Grid a c) = let es' = map (\j -> (j, k (Grid a j))) (indices a)
                          in Grid (array (bounds a) es') c

instance Neighbourhood (Grid Int) where
    type Marker (Grid Int) = Int
    neighbourhood (Grid a i) = 
        let (l, u) = bounds a
        in  if (i == l)               then [(Grid a l, 0), (Grid a (l + 1), 1)]
            else if (i == u)          then [(Grid a (u - 1), -1), (Grid a u, 0)]
            else if (i > l && i < u)  then [(Grid a (i - 1), -1), (Grid a i, 0),
                                            (Grid a (i + 1), 1)]
            else []

{- </grid> -}

-- Example

localMean g = (sum $ neighbours g) / (fromInteger . toInteger $ length (neighbours g))

foo = promote localMean (Grid (listArray (0 :: Int, 4) [1.0, 4.0, 2.0, 4.0, 1.0]) 0)