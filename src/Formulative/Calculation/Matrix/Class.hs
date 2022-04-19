{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.Matrix.Class where

import Control.Exception.Safe
import Data.Complex
import Data.Foldable (foldl')
import Data.Kind
import qualified Data.Matrix.Static.Dense as MSD
import qualified Data.Matrix.Static.Generic as MSG
import qualified Data.Matrix.Static.LinearAlgebra as MSL
import qualified Data.Matrix.Static.LinearAlgebra.Types as MSL
import qualified Data.Matrix.Static.Sparse as MSS
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Singletons
import Data.Type.Equality
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VST

-- import Eigen.Internal (CTriplet (..), Cast (..))
-- import qualified Eigen.Matrix as E
import qualified Numeric.LinearAlgebra as H
import qualified Numeric.LinearAlgebra.Data as HD

-- import qualified Eigen.Solver.LA as LA (Decomposition (HouseholderQR), solve)
-- import qualified Eigen.SparseMatrix as ES

import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Internal.List
import Formulative.Calculation.Internal.TypeLevelList
import Formulative.Calculation.Internal.TypeLevelNatural
import Formulative.Calculation.Matrix.Types
import Formulative.Calculation.VectorSpace.InnerProductSpace
import Formulative.Calculation.VectorSpace.NormSpace
import Formulative.Calculation.VectorSpace.VectorSpace
import Formulative.Preprocess.Exception
import GHC.Exts (IsList (Item, fromList, toList))
import GHC.TypeNats
import Numeric.LinearAlgebra.Devel (mapMatrixWithIndex)

type MatrixKind = Nat -> Nat -> Type -> Type

-- TODO: class MatrixGeneral (mat :: MatrixKind) a の形に直す
class MatrixGeneral (mat :: MatrixKind) r c a where
    imap :: ((Int, Int) -> a -> a) -> mat r c a -> mat r c a
    unsafeIndexMat :: mat r c a -> (Int, Int) -> a
    transpose :: mat r c a -> mat c r a
    identity :: (r ~ c) => mat r r a
    det :: (r ~ c) => mat r r a -> a

-- safeIndexMat :: (SingI r, SingI c, SingI n, SingI m, n <= r, m <= c) => (Sing n, Sing m) -> mat r c a -> a

-- Ax = b
-- x = solveCG a b
-- TODO: 計算失敗時のerror型を定義
class MatrixSolveCG (mat :: MatrixKind) a where
    solveCG :: (KnownNat r, KnownNat c, MonadThrow m) => mat r c a -> mat r 1 a -> m (mat c 1 a)

class MatrixSolveQRUnsafe (mat :: MatrixKind) a where
    solveCGunsafe :: (KnownNat r, KnownNat c) => mat r c a -> mat r 1 a -> mat c 1 a

instance (RealFloat a) => MatrixSolveCG HMatrixSized a where
    solveCG a b = undefined

instance (RealFloat a) => MatrixSolveQRUnsafe HMatrixSized a where
    solveCGunsafe a b = undefined

--------------------------------------------------------------------
-- deriving instance :: hmatrix
--------------------------------------------------------------------

-- unsafeIndexMatE :: forall r c a. (E.Elem a, KnownNat c, KnownNat r) => HMatrixSized r c a -> (Int, Int) -> a
-- unsafeIndexMatE mat (n, m) = case (someNatVal nInteger, someNatVal mInteger) of
--     (SomeNat (_ :: Proxy n), SomeNat (_ :: Proxy m)) -> case (leqNat (Proxy :: Proxy n) (Proxy :: Proxy r), leqNat @Maybe (Proxy :: Proxy m) (Proxy :: Proxy c)) of
--         (Just Refl, Just Refl) -> (E.!) (E.Row :: E.Row n) (E.Col :: E.Col m) mat
--         _ -> error "unsafeIndexMatE"
--   where
--     nInteger = Prelude.fromIntegral n
--     mInteger = Prelude.fromIntegral m

instance (KnownNat r, KnownNat c, H.Field a) => MatrixGeneral HMatrixSized r c a where
    imap f (HMatrixSized x) = HMatrixSized $ mapMatrixWithIndex f x
    transpose (HMatrixSized x) = HMatrixSized $ HD.tr x
    unsafeIndexMat (HMatrixSized x) idx = H.atIndex x idx
    identity = HMatrixSized $ H.ident (fromIntegral $ natVal (Proxy @r))
    det (HMatrixSized x) = H.det x

-- instance (E.Elem a, KnownNat m, KnownNat n) => Additive (HMatrixSized m n a) where
--     (.+.) = E.add
--     zero = E.zero

-- instance (E.Elem a, AdditiveGroup a, KnownNat m, KnownNat n) => AdditiveGroup (HMatrixSized m n a) where
--     (.-.) = E.sub
--     negation = E.map negation

-- instance (E.Elem a, KnownNat l, KnownNat m, KnownNat n) => Mul (HMatrixSized l m a) (HMatrixSized m n a) (HMatrixSized l n a) where
--     (.@.) = E.mul

-- instance (E.Elem a, Field a, KnownNat n, KnownNat m) => Multiplicative (HMatrixSized n m a) where
--     (.*.) x y = mult E.map x y
--     one = E.ones

-- TODO: fill in "undefined"
-- instance (E.Elem a, KnownNat r, KnownNat c) => MatrixGeneral HMatrix r c a where
--     imap f = E.imap (curry f)
--     transpose = E.transpose
--     unsafeIndexMat = unsafeIndexMatE
--     identity = E.identity
--     det = E.determinant

-- instance (E.Elem a, Field a, KnownNat n, KnownNat m) => VectorSpace (HMatrixSized n m a) where
--     type Scalar (HMatrixSized n m a) = a
--     a *. x = E.map (a .*.) x

-- cooToCTriplet :: Cast a => (Int, Int, a) -> CTriplet a
-- cooToCTriplet (i, j, x) = CTriplet (toC i) (toC i) (toC x)

-- instance (E.Elem a, Field a, KnownNat n, KnownNat m) => InnerProductSpace (HMatrixSized n m a) where
--     x <.> y = E.norm (transpose x .@. y)

--------------------------------------------------------------------
-- deriving instance :: matrix-sized
--------------------------------------------------------------------

-- sparse
instance (KnownNat m, KnownNat n, MSL.Numeric a) => Additive (MSL.SparseMatrix m n a) where
    zero = MSS.fromTriplet (V.fromList [])
    a .+. b = a MSL.%+% b

instance (KnownNat m, KnownNat n, MSL.Numeric a, AdditiveGroup a) => AdditiveGroup (MSL.SparseMatrix m n a) where
    negation = MSG.map negation
    a .-. b = a MSL.%-% b

instance forall n m a. (KnownNat m, KnownNat n, MSL.Numeric a, Multiplicative a) => Multiplicative (MSL.SparseMatrix m n a) where
    one = MSD.convertAny (MSD.replicate one :: MSL.Matrix m n a)
    a .*. b = a MSL.%*% b

instance forall l m n a. (MSL.Numeric a, KnownNat l, KnownNat m, KnownNat n) => Mul (MSL.SparseMatrix l m a) (MSL.SparseMatrix m n a) (MSL.SparseMatrix l n a) where
    (.@.) = (MSL.@@)

instance forall n m a. (KnownNat m, KnownNat n, MSL.Numeric a, VectorSpace a, Multiplicative a) => VectorSpace (MSL.SparseMatrix m n a) where
    type Scalar (MSL.SparseMatrix m n a) = a
    (*.) a x = MSG.map (a .*.) x

instance
    forall n m a.
    ( KnownNat m
    , KnownNat n
    , VST.Storable (RealField a)
    , Additive (RealField a)
    , NormSpace a
    , MSL.Numeric a
    , Ord (RealField a)
    , VectorSpace a
    , Multiplicative a
    , Transcendental (RealField a)
    ) =>
    NormSpace (MSL.SparseMatrix m n a)
    where
    type RealField (MSL.SparseMatrix m n a) = RealField a
    norm (Lp p) x = flip (.**.) (reciprocal p) $ VST.foldl' (binaryOpLp (Lp p)) zero $ VST.map (norm (Lp p)) $ MSG.flatten x
    norm LInfinity x = VST.foldl' (binaryOpLp LInfinity) zero $ VST.map (norm LInfinity) $ MSG.flatten x

instance
    {-# OVERLAPS #-}
    ( KnownNat n
    , KnownNat m
    , MSL.Numeric a
    , NormSpace a
    , Multiplicative a
    ) =>
    InnerProductSpace (MSL.SparseMatrix m n a)
    where
    x <.> y = VST.sum $ MSG.takeDiag (MSG.transpose x .@. y)

instance (KnownNat n, KnownNat m) => InnerProductSpace (MSL.SparseMatrix m n (Complex Float)) where
    x <.> y = VST.sum $ MSG.takeDiag (MSG.transpose (MSG.map conjugate x) .@. y)

instance (KnownNat n, KnownNat m) => InnerProductSpace (MSL.SparseMatrix m n (Complex Double)) where
    x <.> y = VST.sum $ MSG.takeDiag (MSG.transpose (MSG.map conjugate x) .@. y)

-- Dense

instance (KnownNat m, KnownNat n, MSL.Numeric a, Additive a) => Additive (MSL.Matrix m n a) where
    zero = MSD.replicate zero
    (.+.) = (MSL.%+%)

instance (KnownNat m, KnownNat n, MSL.Numeric a, AdditiveGroup a) => AdditiveGroup (MSL.Matrix m n a) where
    negation = MSG.map negation
    (.-.) = (MSL.%-%)

instance forall n m a. (KnownNat m, KnownNat n, MSL.Numeric a, Multiplicative a) => Multiplicative (MSL.Matrix m n a) where
    one = MSD.replicate one
    (.*.) = (MSL.%*%)

instance (MSL.Numeric a, KnownNat l, KnownNat m, KnownNat n) => Mul (MSL.Matrix l m a) (MSL.Matrix m n a) (MSL.Matrix l n a) where
    (.@.) = (MSL.@@)

instance forall n m a. (KnownNat m, KnownNat n, MSL.Numeric a, VectorSpace a, Multiplicative a) => VectorSpace (MSL.Matrix m n a) where
    type Scalar (MSL.Matrix m n a) = a
    (*.) a x = MSG.map (a .*.) x

instance forall n m a. (KnownNat m, KnownNat n, VST.Storable (RealField a), Additive (RealField a), NormSpace a, MSL.Numeric a, Ord (RealField a), VectorSpace a, Multiplicative a, Transcendental (RealField a)) => NormSpace (MSL.Matrix m n a) where
    type RealField (MSL.Matrix m n a) = RealField a
    norm (Lp p) x = flip (.**.) (reciprocal p) $ VST.foldl' (binaryOpLp (Lp p)) zero $ VST.map (norm (Lp p)) $ MSG.flatten x
    norm LInfinity x = VST.foldl' (binaryOpLp LInfinity) zero $ VST.map (norm LInfinity) $ MSG.flatten x

instance (MSL.Numeric a, NormSpace a, Multiplicative a, KnownNat n, KnownNat m) => InnerProductSpace (MSL.Matrix m n a) where
    x <.> y = VST.sum $ MSG.flatten (MSG.transpose x .@. y)