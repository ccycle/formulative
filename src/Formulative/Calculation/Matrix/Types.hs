module Formulative.Calculation.Matrix.Types where

import Data.Proxy
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.VectorSpace.Class
import GHC.Generics
import GHC.TypeNats
import Numeric.LinearAlgebra (Container, Element, Matrix, Numeric, Vector)
import qualified Numeric.LinearAlgebra as H

newtype HVectorSized (n :: Nat) a = HVectorSized (Vector a)
    deriving stock (Generic)

newtype HMatrixSized (n :: Nat) (m :: Nat) a = HMatrixSized (Matrix a)
    deriving stock (Generic, Show)

type HMatrixElement a = (Container Matrix a, Container Vector a, Numeric a, H.Field a, Num (Vector a))

-- TODO: testを書く
hConcat :: (Element a) => HMatrixSized n m a -> HMatrixSized n l a -> HMatrixSized n (m + l) a
hConcat (HMatrixSized x) (HMatrixSized y) = HMatrixSized $ x H.||| y

vConcat :: (Element a) => HMatrixSized n m a -> HMatrixSized l m a -> HMatrixSized (n + l) m a
vConcat (HMatrixSized x) (HMatrixSized y) = HMatrixSized $ x H.=== y

dropLastRow :: Element a => HMatrixSized n m a -> HMatrixSized (n -1) m a
dropLastRow (HMatrixSized x) = HMatrixSized $ x H.?? (H.DropLast 1, H.All)

instance (KnownNat n, KnownNat m, Num a, Container Matrix a, Container Vector a, Num (Vector a)) => Additive (HMatrixSized n m a) where
    HMatrixSized x .+. HMatrixSized y = HMatrixSized (x + y)
    zero = HMatrixSized (H.konst 0 (nInt, mInt))
      where
        nInt = fromIntegral $ natVal (Proxy @n)
        mInt = fromIntegral $ natVal (Proxy @m)

instance (KnownNat n, KnownNat m, Num a, Container Matrix a, Container Vector a, Num (Vector a)) => AdditiveGroup (HMatrixSized n m a) where
    negation = undefined
    HMatrixSized x .-. HMatrixSized y = HMatrixSized (x - y)

instance (KnownNat n, KnownNat m, Num a, Container Matrix a, Container Vector a, Num (Vector a)) => Multiplicative (HMatrixSized n m a) where
    HMatrixSized x .*. HMatrixSized y = HMatrixSized (x * y)
    one = HMatrixSized (H.konst 1 (nInt, mInt))
      where
        nInt = fromIntegral $ natVal (Proxy @n)
        mInt = fromIntegral $ natVal (Proxy @m)

instance (Numeric a) => Mul (HMatrixSized n m a) (HMatrixSized m l a) (HMatrixSized n l a) where
    (.@.) (HMatrixSized x) (HMatrixSized y) = HMatrixSized (x H.<> y)

instance (Numeric a, KnownNat n, KnownNat m, Num (Vector a)) => VectorSpace (HMatrixSized n m a) where
    type Scalar (HMatrixSized n m a) = a
    (*.) x (HMatrixSized y) = HMatrixSized (H.cmap (x *) y)

instance (Numeric a, KnownNat n, KnownNat m, Num (Vector a)) => InnerProductSpace (HMatrixSized n m a) where
    (<.>) (HMatrixSized x) (HMatrixSized y) = undefined -- (x .@. y)