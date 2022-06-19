module Formulative.Calculation.Algebra.ComplexNumber (
    module Data.Complex,
    module Formulative.Calculation.Algebra.ComplexNumber,
) where

import Data.Complex hiding (conjugate)
import Formulative.Calculation.Algebra.Arithmetic.Class
import Prelude hiding (fromInteger)
import qualified Prelude

instance Additive a => Additive (Complex a)
instance AdditiveGroup a => AdditiveGroup (Complex a)

instance (AdditiveGroup a, Multiplicative a) => Multiplicative (Complex a) where
    (:+) a1 a2 .*. (:+) b1 b2 = (:+) (a1 .*. b1 .-. a2 .*. b2) (a2 .*. b1 .+. a1 .*. b2)
    one = (:+) one zero
instance (Semiring a, AdditiveGroup a) => Semiring (Complex a) where
    fromInteger x = (:+) (fromInteger x) (fromInteger x)

instance (Ring a) => Ring (Complex a)

instance (Eq a, Field a) => Field (Complex a) where
    reciprocal (((:+) a b)) = (:+) (a ./. (a .*. a .+. b .*. b)) (negation b ./. (a .*. a .+. b .*. b))
    fromRational' a = (:+) (fromRational' a) zero

conjugate (x :+ y) = x :+ negation y