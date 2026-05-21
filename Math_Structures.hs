module Algebra where

import Prelude hiding (Monoid, Semigroup, (<>), (+), (*), id)

-- Objects
-- Things that exist in our categoric universe
class Obj o
-- Arrows
-- Represents the rawest form of connection between any two things.
data Arrow q x y where
    Arrow :: q Obj x Obj y -> Arrow q Obj x Obj y
-- Morphisms
-- Represents arrows with composition
data Morphism q x y where
    Morphism   :: q Obj x Obj y -> Morphism q Obj x Obj y
    ComposeMorphism :: Morphism q Obj y Obj z -> Morphism q Obj x Obj y -> Morphism q Obj x Obj z
data Endomorphism q x y where
    Endomorphism   :: q Obj x Obj x -> Endomorphism q Obj x Obj x
    ComposeEndomorphism :: Endomorphism q Obj x Obj x -> Endomorphism q Obj x Obj x -> Endomorphism q Obj x Obj x

data T v = T

-- Quivers
-- The base structure defining the "shape" of the algebra.
-- 'q' is a type constructor representing the arrows between objects.
class Quiver q where
    -- This class provides the structure for arrows: edge start end
    -- It ensures that every edge has a defined domain and codomain.
    source :: (Obj x, Obj y) => Arrow q x y -> T x
    source _ = T
    target :: (Obj x, Obj y) => Arrow q x y -> T y
    target _ = T

-- Magmoids
-- A Quiver + a partial binary operation.
-- Inherits from Quiver but then equips with an operation.
class Quiver q => Magmoid q where
    composeMagmoid :: (Obj x, Obj y, Obj z) => Arrow q y z -> Arrow q x y -> Arrow q x z



-- Semigroupoids
-- A Magmoid with associativity
-- Inherits from Magmoid and gets endowed with associativity.
class Magmoid q => Semigroupoid q where
    composeSemigroupoid :: Arrow q y z -> Arrow q x y -> Arrow q x z
    composeSemigroupoid (Arrow f) (Arrow g) = Arrow (f . g)

-- | Endomorphism
-- Represents an element as an Endomorphism (o -> o).
newtype Endo q o = Endo { getEndo :: Arrow q o o }

newtype Id q x = Id { getId :: Arrow q x x }
-- Categories
-- A Semigroupoid with identity
-- Inherits from Semigroupoid and gets endowed with an identity morphism.
class (Semigroupoid q) => Category q where
    id :: (Obj o) => Endo q o

-- Magmas
-- A Quiver + a total binary operation.
-- Inherits from Magmoid but fixes the vertices to a single object 'o'.
class (Magmoid q, Obj o) => Magma q o where
    (⋆) :: Endo q o o -> Endo q o o -> Endo q o o
    (Endo f) ⋆ (Endo g) = Endo { getEndo = f ∘ g }

-- Semigroups
-- A Magma with associativity
-- Inherits from Magma but endowed with associativity
class Magma q o => Semigroup q o where
    (Endo f) ⋆ (Endo g) = Endo { getEndo = f (getEndo g) }


-- Monoids
-- A Category + a total binary operation.
-- Inherits from Category but fixes the vertices to a single object 'o'.
class (Category q o) => Monoid q o where

