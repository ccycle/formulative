module Test.NonHomogeneousProd where

import Data.Kind
import Formulative.Calculation.Matrix.Class
import GHC.TypeNats
import Test.Tasty

class NonHomogeneousMultiplicative a b where
    type Mul a b
    (.@.) :: a -> b -> Mul a b

instance (KnownNat l, KnownNat m, KnownNat n) => NonHomogeneousMultiplicative ((mat :: MatrixKind) l m a) ((mat :: MatrixKind) m n a) where
    type Mul (mat l m a) (mat m n a) = mat l n a
    (.@.) = undefined
