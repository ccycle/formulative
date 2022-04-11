module Formulative.Calculation.Algebra.HyperComplex where

class HyperComplex m a where
    getRealPart :: m a -> a
    getDualPart :: m a -> a

-- applyD :: m (a -> b) -> a -> m b

-- applyD (DualNumber f g) x = DualNumber (f x) (g x)
