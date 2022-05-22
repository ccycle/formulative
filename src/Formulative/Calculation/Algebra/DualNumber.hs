{-# LANGUAGE DeriveAnyClass #-}

module Formulative.Calculation.Algebra.DualNumber where

import Formulative.Calculation.Algebra.Arithmetic.Class hiding (Transcendental (..))
import GHC.Generics
import Refined.Unsafe.Type
import Prelude hiding (fromInteger)
import qualified Prelude

data DualNumber a = DualNumber !a !a
    deriving stock (Generic, Show, Eq)
    deriving anyclass (Additive, AdditiveGroup)

data GeneralizedDualNumber a b = GeneralizedDualNumber a b
    deriving stock (Generic, Show, Eq)
    deriving anyclass (Additive, AdditiveGroup)

type D2 a = GeneralizedDualNumber a (DualNumber a)

-- instance (Num a) => Num (DualNumber a) where
--     fromInteger a = DualNumber (Prelude.fromInteger a) 0

instance (Additive a, Multiplicative a) => Multiplicative (DualNumber a) where
    DualNumber a a' .*. DualNumber b b' = DualNumber (a .*. b) (a' .*. b .+. a .*. b')
    one = DualNumber one zero

instance Semiring a => Semiring (DualNumber a)
instance Ring a => Ring (DualNumber a)

instance (Eq a, Field a) => Field (DualNumber a) where
    reciprocal (DualNumber a a') = DualNumber a (negation $ a' .*. (a .*. a))
    fromRational' a = DualNumber (fromRational' a) zero
    isDividable (DualNumber a a') = a == zero

liftD a = DualNumber a a

applyD (DualNumber f g) x = DualNumber (f x) (g x)

epsilon :: Num a => DualNumber a
epsilon = DualNumber 0 1

-- liftD ::(Num a1, Num a2) =>  (a1->a2 )-> (DualNumber a1 -> DualNumber a2)
-- liftD f = fmap f

getDualPart (DualNumber _ y) = y

diff :: forall a. Floating a => (forall x. Floating x => x -> x) -> a -> a
diff f x = getDualPart $ f (DualNumber x 1)

instance Num a => Num (DualNumber a) where
    (DualNumber x x') + (DualNumber y y') = DualNumber (x + y) (x' + y')
    (DualNumber x x') * (DualNumber y y') = DualNumber (x * y) (x' * y + x * y')
    negate (DualNumber x x') = DualNumber (negate x) (negate x')
    abs (DualNumber x x') = DualNumber (abs x) (x' * signum x)
    signum (DualNumber x x') = DualNumber (signum x) 0
    fromInteger n = DualNumber (Prelude.fromInteger n) 0

instance Fractional a => Fractional (DualNumber a) where
    recip (DualNumber x x') = DualNumber (recip x) (-1 * x' * recip (x * x))
    fromRational x = DualNumber (fromRational x) 0

instance Floating a => Floating (DualNumber a) where
    pi = DualNumber pi 0
    exp (DualNumber x x') = DualNumber (exp x) (x' * exp x)
    log (DualNumber x x') = DualNumber (log x) (x' / x)
    sin (DualNumber x x') = DualNumber (sin x) (x' * cos x)
    cos (DualNumber x x') = DualNumber (cos x) (- x' * sin x)
    asin (DualNumber x x') = DualNumber (asin x) (x' / sqrt (1 - x ^ 2))
    acos (DualNumber x x') = DualNumber (acos x) (- x' / sqrt (1 - x ^ 2))
    atan (DualNumber x x') = DualNumber (atan x) (x' / (1 + x ^ 2))
    sinh (DualNumber x x') = DualNumber (sinh x) (x' * cosh x)
    cosh (DualNumber x x') = DualNumber (cosh x) (x' * sinh x)
    asinh (DualNumber x x') = DualNumber (asinh x) (x' / sqrt (1 + x ^ 2))
    acosh (DualNumber x x') = DualNumber (acosh x) (x' / sqrt (x ^ 2 - 1))
    atanh (DualNumber x x') = DualNumber (atanh x) (x' / (1 - x ^ 2))

newtype AD s a = AD {runAD :: a}
    deriving newtype (Eq, Ord, Show, Read, Bounded, Num, Real, Fractional, Floating, Enum, RealFrac, RealFloat)
liftAD :: Floating a => a -> AD s (DualNumber a)
liftAD = AD . liftD

diffD :: (Floating a) => (DualNumber a -> DualNumber a) -> a -> a
diffD f x = getDualPart $ f (DualNumber x 1)

diffAD :: Floating a => (forall s. AD s (DualNumber a) -> AD s (DualNumber a)) -> (a -> a)
diffAD f = diffD (runAD . f . AD)

nthDiff :: (Floating a, Integral b) => (forall x. Floating x => x -> x) -> b -> a -> a
nthDiff f n
    | n == 0 = f
    | n < 0 = error "input number is negative"
    | otherwise = diffAD (nthDiff f (n - 1))