module Formulative.Calculation.Algebra.Arithmetic.Algebraic where

import Formulative.Calculation.Algebra.Arithmetic.Field ( Field )
import qualified Prelude as P

import Data.Ratio (Rational, denominator, numerator, (%))
import Formulative.Calculation.Algebra.Arithmetic.Multiplicative
    ( Multiplicative((.^)) )

class (Field a) => C a where
    {-# MINIMAL root | (^%) #-}

    -- sqrt x  =  x ** (1/2)
    sqrt :: a -> a
    sqrt = root 2

    -- root n x  =  x ** (1/n)
    root :: P.Integer -> a -> a
    root n x = x ^% (1 % n)

    -- x ^% (n/m) = (x^n) ** (1/m)
    (^%) :: a -> Rational -> a
    x ^% y = root (denominator y) (x .^ numerator y)
