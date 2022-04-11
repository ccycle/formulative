module Formulative.Calculation.Algebra.Arithmetic.Mul where

import Control.Algebra
import Control.Applicative
import Formulative.Calculation.Algebra.Arithmetic.Multiplicative

-- Multiplication of Different Types
class Mul a b c | a b -> c, a c -> b, b c -> a where
    (.@.) :: a -> b -> c

    infixl 7 .@.

(<.@.>) :: forall sig m a b c. (Algebra sig m, Mul a b c) => m a -> m b -> m c
(<.@.>) = liftA2 (.@.)
infixl 7 <.@.>