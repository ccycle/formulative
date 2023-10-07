{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Physics.Units.Arithmetic (
    module X,
    type (>*<),
    type (>/<),
    type (^+),
    type (^-),
    (>*<),
    (>/<),
    (*<),
    (>/),
    (/<),
    Pretty,
    NthRoot,
    nthRoot,
    SquareRoot,
    hypercube,
    squareRoot,
    Square,
    square,
    Cube,
    cube,
    Tesseract,
    tesseract,
    Penteract,
    penteract,
) where

import Data.Coerce
import Data.Data
import Data.Kind (Type)
import Formulative.Algebra.Prelude hiding (
    (>*<),
    (>/<),
 )
import GHC.TypeNats
import Physics.Units as X (Exponent (..), N1, N2, N3, N4, P1, P2, P3, P4, ShowUnit, Z)
import Physics.Units.Arithmetic as X (
    type Divide,
    type IsInteger,
    type Minus,
    type Negate,
    type Plus,
 )

type family Pretty (d :: Type -> Type) :: Type -> Type
type instance Pretty d = d

(>*<) :: (Multiplicative x, Coercible (f x) x, Coercible (f' x) x, Applicative (f >*< f')) => f x -> f' x -> (f >*< f') x
x >*< y = pure (coerce x * coerce y)
(>/<) :: (Field x, Coercible (f x) x, Coercible (f' x) x, Applicative (f >/< f')) => f x -> f' x -> (f >/< f') x
x >/< y = pure (coerce x / coerce y)

(*<) :: (Multiplicative x, Functor f, z ~ f x) => x -> z -> z
x *< y = fmap (x *) y
(>/) :: (Field x, Functor f, z ~ f x) => z -> x -> z
x >/ y = fmap (/ y) x
(/<) :: (Field x, Functor f, Coercible (f x) ((f ^- 1) x)) => x -> f x -> (f ^- 1) x
x /< y = coerce (fmap (x /) y)

infixl 7 >*<, >/<, *<, /<, >/

type family (^+) d n where
    d ^+ 0 = d >/< d
    d ^+ n = d >*< d ^+ (n - 1)

type family (^-) d n where
    d ^- n = d >/< d ^+ (n + 1)

infixr 8 ^+, ^-

type family NthRoot (n :: Nat) (d :: Type -> Type) :: Type -> Type
nthRoot :: (KnownNat n, Functor f, Coercible (f x) (NthRoot n f x), Algebraic x) => Proxy n -> f x -> NthRoot n f x
nthRoot p = coerce . fmap (root (naturalToInteger $ natVal p))

type SquareRoot d = NthRoot 2 d
squareRoot :: (Functor f, Coercible (f x) (SquareRoot f x), Algebraic x) => f x -> SquareRoot f x
squareRoot = nthRoot (Proxy :: Proxy 2)

hypercube :: (KnownNat n, Functor f, Coercible (f x) ((f ^+ n) x), Multiplicative x) => Proxy n -> f x -> (f ^+ n) x
hypercube p = coerce . fmap (^+ natVal p)

type Square d = d ^+ 2
square :: (Coercible (f x) (Square f x), Multiplicative x, Functor f) => f x -> Square f x
square = hypercube (Proxy :: Proxy 2)

type Cube d = d ^+ 3
cube :: (Coercible (f x) (Cube f x), Algebraic x, Functor f) => f x -> Cube f x
cube = hypercube (Proxy :: Proxy 3)

type Tesseract d = d ^+ 4
tesseract :: (Coercible (f x) (Tesseract f x), Algebraic x, Functor f) => f x -> Tesseract f x
tesseract = hypercube (Proxy :: Proxy 4)

type Penteract d = d ^+ 5
penteract :: (Coercible (f x) (Penteract f x), Algebraic x, Functor f) => f x -> Penteract f x
penteract = hypercube (Proxy :: Proxy 5)
