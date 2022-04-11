module Formulative.Calculation.Algebra.Arithmetic.Div where

import Control.Algebra
import Control.Applicative
import Formulative.Calculation.Algebra.Arithmetic.Multiplicative

-- Division of Different Types
class Div a b c | a b -> c, a c -> b, b c -> a where
    (.//.) :: a -> b -> c

    infixl 7 .//.

-- (<.@.>) :: forall sig m a b c. (Algebra sig m, Div a b c) => m a -> m b -> m c
-- (<.@.>) = liftA2 (.@.)
-- infixl 7 <.@.>