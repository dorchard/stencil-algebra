{-# LANGUAGE TypeFamilies, FlexibleInstances, NoMonomorphismRestriction, 
             MultiParamTypeClasses #-}

import GHC.Arr

{- All containers must be containers for stencil computations -}

import Control.Comonad

{- Provides:
class Functor c => Comonad c where
   extract :: c a -> a
   extend :: (c a -> b) -> c a -> c b
-}

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

data Grid i a = Grid (Array i a) i (i, i) deriving Show

instance Ix i => Functor (Grid i) where
    fmap f (Grid a i b) = Grid (amap f a) i b

instance Ix i => Comonad (Grid i) where
    extract (Grid a c b) = a ! c
    extend k (Grid a c b) = let es' = map (\j -> (j, k (Grid a j b))) (indices a)
                            in Grid (array (bounds a) es') c b

instance Neighbourhood (Grid Int) where
    type Marker (Grid Int) = Int
    neighbourhood (Grid a i b) = 
        let (l, u) = bounds a
        in  if (i == l)               then [(Grid a l b, 0), (Grid a (l + 1) b, 1)]
            else if (i == u)          then [(Grid a (u - 1) b, -1), (Grid a u b, 0)]
            else if (i > l && i < u)  then [(Grid a (i - 1) b, -1), (Grid a i b, 0),
                                            (Grid a (i + 1) b, 1)]
            else []

{- </grid> -}

-- Example

localMean g = (sum $ neighbours g) / (fromInteger . toInteger $ length (neighbours g))

example1 = promote localMean (Grid (listArray (0 :: Int, 4) [1.0, 4.0, 2.0, 4.0, 1.0]) 0 (0, 4))

class SubspaceComonad c i where
    {- Relative comonad on the category of endomorphisms -}
    promoteSub :: (c i a -> a) -> c i a -> c i a

    {- The following are comonad homomorphisms, restricting the context domain -}

    -- Restrict the internal bounds to a subspace defined by a pair of lower-upper bounds
    subspace :: c i a -> (i, i) -> c i a
                
    -- Drop the restriction of a subspace, go back to the full spaced
    fullspace :: c i a -> c i a

instance SubspaceComonad Grid Int where
    promoteSub k (Grid a c (l, u)) = 
           let es' = map (\j -> if (elem j [l..u]) then 
                                    (j, k (Grid a j (l, u)))
                                else 
                                    (j, a ! j)) (indices a)
           in Grid (array (bounds a) es') c (l, u)

    subspace (Grid a i _) b = Grid a i b
    fullspace (Grid a i _)  = Grid a i (bounds a)
        
    
