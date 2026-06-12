{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}

module Algebra where

import Prelude hiding (Monoid, Semigroup, (<>), (+), (*), id)

-- Objects
-- Things that exist in our categoric universe
class Obj o
instance Obj (Morphism q)

-- Arrows
-- Represents the rawest form of connection between any two things.
data Arrow q x y where
    SetArrow :: (Obj x, Obj y) => q x y -> Arrow q x y

-- Morphisms
-- Represents arrows with composition
data Morphism q x y where
    Id :: Obj x => Morphism q x x
    Morph :: Arrow q x y -> Morphism q x y
    ComposeMorphism :: Morphism q y z -> Morphism q x y -> Morphism q x z
    AssocChain  :: Obj x => [Morphism q x x] -> Morphism q x x

data Endomorphism q x where
    Endo :: Morphism q x x -> Endomorphism q x

data Isomorphism q x y where
    -- if the inverse (Morphism q y x) is defined, there is proof that we have an isomophism
    Iso :: Morphism q x y -> Morphism q y x -> Isomorphism q x y

-- Objects endowed with a partial binary operation
class Obj m => Magmoid m where
    magmoidOp :: (Obj x, Obj y, Obj z) => m y z -> m x y -> m x z
instance Magmoid (Morphism q) where
    magmoidOp :: (Obj x, Obj y, Obj z) => Morphism q y z -> Morphism q x y -> Morphism q x z
    magmoidOp f g = ComposeMorphism f g

-- Objects endowed with a total binary operation
class Magmoid m => Magma m where
    magmaOp :: (Obj x) => m x x -> m x x -> m x x
instance Magma (Morphism q) where
    magmaOp :: Obj x => Morphism q x x -> Morphism q x x -> Morphism q x x
    magmaOp f g = ComposeMorphism f g

-- Objects endowed with a total binary operation and associative
class Magma m => Semigroup m where
    semigroupOp :: Obj x => m x x -> m x x -> m x x
    semigroupOp = magmaOp
instance Semigroup (Morphism q) where
    semigroupOp f g = AssocChain (unwrap f ++ unwrap g)
        where
            unwrap (AssocChain xs) = xs
            unwrap x = [x]


--class Semigroup g => Monoid g where
--    mempty :: g
--class Monoid g => Group g where
--    groupOp :: g -> g -> g
--    invert :: g -> g
--    identity :: g
