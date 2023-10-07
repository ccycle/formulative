module Formulative.Algebra.Number.Complex where

import Formulative.Algebra.Arithmetic
import Formulative.Algebra.Prelude
import GHC.Generics (Generic)

data Complex a = a :+ a deriving stock (Show, Eq, Generic)

instance Additive a => Additive (Complex a)
instance AdditiveGroup a => AdditiveGroup (Complex a)

instance (AdditiveGroup a, Multiplicative a) => Multiplicative (Complex a) where
    (:+) a1 a2 * (:+) b1 b2 = (:+) (a1 * b1 - a2 * b2) (a2 * b1 + a1 * b2)
    one = (:+) one zero
instance (Ring a) => Semiring (Complex a)
instance (FromInteger a, Additive a) => FromInteger (Complex a) where
    fromInteger x = (:+) (fromInteger x) zero
instance (Ring a) => Ring (Complex a)

instance (Eq a, Field a) => Field (Complex a) where
    recip (((:+) a b)) = (:+) (a / (a * a + b * b)) (-b / (a * a + b * b))
    fromRational a = (:+) (fromRational a) zero

conjugate :: AdditiveGroup a => Complex a -> Complex a
conjugate (x :+ y) = x :+ negate y