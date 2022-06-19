module Formulative.Calculation.Algebra.MaxPlus where

import Data.Coerce
import Formulative.Calculation.Algebra.Arithmetic.Additive
import Formulative.Calculation.Algebra.Arithmetic.Multiplicative
import Formulative.Calculation.Algebra.Arithmetic.Semiring
import GHC.Generics
import Prelude hiding (fromInteger)

data MaxPlus a = MaxPlus a | NegativeInf
    deriving stock (Eq, Generic)

instance (Show a) => Show (MaxPlus a) where
    show (MaxPlus a) = show a
    show NegativeInf = "-oo"

instance (Ord a) => Ord (MaxPlus a) where
    compare a NegativeInf = GT
    compare NegativeInf a = LT
    compare (MaxPlus a) (MaxPlus b) = compare a b
    (<=) a NegativeInf = False
    (<=) NegativeInf a = True
    (<=) (MaxPlus a) (MaxPlus b) = (<=) a b

instance (Ord a) => Additive (MaxPlus a) where
    (.+.) = coerce . max
    zero = NegativeInf

instance (Ord a, Additive a) => Multiplicative (MaxPlus a) where
    (.*.) (MaxPlus a) (MaxPlus b) = MaxPlus ((.+.) a b)
    (.*.) NegativeInf a = NegativeInf
    (.*.) a NegativeInf = NegativeInf
    one = MaxPlus zero

instance (Ord a, Semiring a) => Semiring (MaxPlus a) where
    fromInteger i = MaxPlus $ fromInteger i
