module Formulative.Calculation.VectorSpace.Tensor where

import Data.Proxy
import GHC.TypeNats

-- V: Vector C: Covector
data ShapeNat = C Nat | V Nat

type family DualIndexT a where
    DualIndexT (C n) = V n
    DualIndexT (V n) = C n

type ShapeT = [ShapeNat]

data Tensor (s :: ShapeT) a where
    ScalarT :: Tensor '[] a
    ConsVectorT :: KnownNat n => Tensor '[V n] a -> Tensor s a -> Tensor (V n : s) a
    ConsCovectorT :: KnownNat n => Tensor '[C n] a -> Tensor s a -> Tensor (C n : s) a

contractionT :: Tensor (C n : s) a -> Tensor (s) a -> Tensor s a
contractionT = undefined

-- 共変、反変の区別をつけたい
-- Vector n (Tensor s a) -> Tensor (C n:s) a
-- Covector n (Tensor s a) -> Tensor (C n:s) a