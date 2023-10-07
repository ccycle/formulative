module Formulative.Algebra.Number.IntMod where

import Data.Proxy
import Formulative.Algebra.Prelude
import GHC.TypeNats
import Prelude hiding (fromInteger, (-))

newtype IntMod (n :: Nat) = IntMod Integer

instance KnownNat n => Show (IntMod n) where
  show t@(IntMod x) = show x ++ " (mod " ++ show (natVal t) ++ ")"

instance (KnownNat n) => Eq (IntMod n) where
  IntMod i == IntMod j = (i - j) `mod` n == 0
   where
    n = fromIntegral $ natVal (Proxy :: Proxy n)

deriving newtype instance Additive (IntMod n)
deriving newtype instance AdditiveGroup (IntMod n)
deriving newtype instance Multiplicative (IntMod n)
deriving newtype instance Semiring (IntMod n)
deriving newtype instance Ring (IntMod n)