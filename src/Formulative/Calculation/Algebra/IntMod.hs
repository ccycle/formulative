module Formulative.Calculation.Algebra.IntMod where

import Data.Proxy
import Data.Ratio
import Formulative.Calculation.Algebra.Arithmetic.Additive
import Formulative.Calculation.Algebra.Arithmetic.AdditiveGroup
import Formulative.Calculation.Algebra.Arithmetic.Multiplicative
import Formulative.Calculation.Algebra.Arithmetic.Ring
import Formulative.Calculation.Algebra.Arithmetic.Semiring
import GHC.TypeNats

newtype IntMod (n :: Nat) = IntMod Integer

instance KnownNat n => Show (IntMod n) where
  show t@(IntMod x) = show x ++ " (mod " ++ show (natVal t) ++ ")"

instance (KnownNat n) => Eq (IntMod n) where
  IntMod i == IntMod j = (i .-. j) `mod` n == 0
   where
    n = fromIntegral $ natVal (Proxy :: Proxy n)

deriving newtype instance Additive (IntMod n)
deriving newtype instance AdditiveGroup (IntMod n)
deriving newtype instance Multiplicative (IntMod n)
deriving newtype instance Semiring (IntMod n)
deriving newtype instance Ring (IntMod n)