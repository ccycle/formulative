module Formulative.Calculation.Algebra.MaxPlus where

import Data.Coerce
import Formulative.Calculation.Algebra.Arithmetic.Additive
import Formulative.Calculation.Algebra.Arithmetic.Multiplicative

data MaxPlus a = MaxPlus a | NegativeInf deriving (Eq)

instance (Show a) => Show (MaxPlus a) where
    show (MaxPlus a) = "MaxPlus " <> show a
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
