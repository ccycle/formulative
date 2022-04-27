{-# LANGUAGE DeriveAnyClass #-}

module Formulative.Calculation.Algebra.ComplexNumber where

import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Algebra.HyperComplex
import GHC.Generics
import Prelude hiding (fromInteger)
import qualified Prelude

data ComplexNumber a = ComplexNumber a a deriving (Show, Eq, Generic, Additive, AdditiveGroup)

instance (AdditiveGroup a, Multiplicative a) => Multiplicative (ComplexNumber a) where
    ComplexNumber a1 a2 .*. ComplexNumber b1 b2 = ComplexNumber (a1 .*. b1 .-. a2 .*. b2) (a2 .*. b1 .+. a1 .*. b2)
    one = ComplexNumber one zero

instance (Semiring a, AdditiveGroup a) => Semiring (ComplexNumber a) where
    fromInteger x = ComplexNumber (fromInteger x) (fromInteger x)

instance (Ring a) => Ring (ComplexNumber a)

-- instance (AdditiveGroup a, Multiplicative a, Field a) => Field (ComplexNumber a) where
--     reciprocal (Refined (ComplexNumber a b)) = Refined $ ComplexNumber (a ./. Refined (a .*. a .+. b .*. b)) (negation b ./. Refined (a .*. a .+. b .*. b))
instance (Eq a, Field a) => Field (ComplexNumber a) where
    reciprocal ((ComplexNumber a b)) = ComplexNumber (a ./. (a .*. a .+. b .*. b)) (negation b ./. (a .*. a .+. b .*. b))
    fromRational' a = ComplexNumber (fromRational' a) zero

instance HyperComplex ComplexNumber a where
    getRealPart (ComplexNumber a b) = a
    getDualPart (ComplexNumber a b) = b
