module Formulative.Algebra.Number.MaxPlus where

import Data.Coerce
import Formulative.Algebra.Prelude
import GHC.Generics

data MaxPlus a = MaxPlus a | NegativeInf
    deriving stock (Eq, Generic)

instance (Show a) => Show (MaxPlus a) where
    show (MaxPlus a) = show a
    show NegativeInf = "-oo"

instance (Ord a) => Ord (MaxPlus a) where
    compare _ NegativeInf = GT
    compare NegativeInf _ = LT
    compare (MaxPlus a) (MaxPlus b) = compare a b
    (<=) _ NegativeInf = False
    (<=) NegativeInf _ = True
    (<=) (MaxPlus a) (MaxPlus b) = (<=) a b

instance (Ord a) => Additive (MaxPlus a) where
    (+) = coerce . max
    zero = NegativeInf

instance (Ord a, Additive a) => Multiplicative (MaxPlus a) where
    (*) (MaxPlus a) (MaxPlus b) = MaxPlus ((+) a b)
    (*) NegativeInf _ = NegativeInf
    (*) _ NegativeInf = NegativeInf
    one = MaxPlus zero

instance (Ord a, Semiring a) => Semiring (MaxPlus a)

instance (FromInteger a) => FromInteger (MaxPlus a) where
    fromInteger i = MaxPlus (fromInteger i)
