module Formulative.Calculation.Matrix.Types where

import Conduit
import Data.Csv (ToField)
import qualified Data.Matrix.Static.Dense as MSD
import qualified Data.Matrix.Static.Generic as MSG
import qualified Data.Matrix.Static.LinearAlgebra as MSL
import Data.Matrix.Static.Sparse (toTriplet)
import qualified Data.Matrix.Static.Sparse as MSS
import Data.Proxy
import qualified Data.Vector.Storable as VST
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Internal.List
import Formulative.Calculation.Internal.Types
import Formulative.Calculation.VectorSpace.Class
import Formulative.Postprocess.Export.CSV
import Formulative.Postprocess.Export.Variable.Class
import Formulative.Postprocess.Export.Variable.Local
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

instance (MSG.Matrix mat v a, KnownNat r) => UnsafeIndex (mat r 1 v a) where
    unsafeIndex mat i = MSD.unsafeIndex mat (i, 0)

instance (Element a, KnownNat m, KnownNat n) => IsList (HMatrixSized m n a) where
    type Item (HMatrixSized m n a) = [a]
    fromList = HMatrixSized . H.fromLists
    toList (HMatrixSized x) = H.toLists x

instance (MSG.Matrix mat v a, KnownNat r, KnownNat c) => IsList (mat r c v a) where
    type Item (mat r c v a) = a
    fromList = MSG.fromList
    toList = MSG.toList

instance (KnownNat m, VST.Storable a, MSS.Zero a) => IsVector (VST.Vector a) (MSL.SparseMatrix m 1 a) where
    fromVector = MSS.fromVector
    toVector = flip MSS.unsafeTakeColumn 0

deriving via (MyMatrix (MSL.SparseMatrix p1 p2 a)) instance ToVariableType (MSL.SparseMatrix p1 p2 a)
deriving via (MyMatrix (MSL.Matrix p1 p2 a)) instance ToVariableType (MSL.Matrix p1 p2 a)

instance
    ( ToField a
    , VST.Storable a
    , KnownNat k1
    , KnownNat k2
    ) =>
    ToLazyField (MSL.SparseMatrix k1 k2 a)
    where
    toLazyField x = runConduitPure $ toTriplet x .| mapC (\x -> encodeLF [x]) .| foldlC (<>) ""