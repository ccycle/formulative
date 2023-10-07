{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Types (
    module Formulative.Calculation.DiscreteExteriorCalculus.Geometry.Types,
    module Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Types,
) where

import Control.Applicative (liftA2)
import Data.Coerce
import Data.Constraint
import Data.Hashable
import qualified Data.Matrix.Static.LinearAlgebra.Types as MSL
import Data.Singletons.Prelude.List (SList)
import Data.Singletons.TH hiding (type (<=))
import qualified Data.Text as T
import qualified Data.Vector.Storable as VST
import Dhall
import Formulative.Calculation.Algebra.Arithmetic
import Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Proofs
import Formulative.Calculation.DiscreteExteriorCalculus.Geometry.Types
import Formulative.Calculation.Internal.Types
import Formulative.Calculation.VectorSpace.Class
import Formulative.Postprocess.Export.Variable.Class
import Formulative.Preprocess.DefaultValue
import GHC.TypeNats
import Unsafe.Coerce

-- k-form -> k+1
-- emptyに対応させるため、数字を一個足す
-- 基底部は n=0 (empty)

type DegreeOfForm = Nat

-- TODO: Show instanceを変える
-- 内部表現ではk+1にしているのをkに変える
-- 例: "repmat: primal k-form -> dual l-form" "differential dual l-form"
-- repmatの次数を見て場合分け
newtype DECrepresentationMatrix (n :: Nat) (l :: [Nat]) (c1 :: CellType) (k1 :: DegreeOfForm) (c2 :: CellType) (k2 :: DegreeOfForm) a = DECrepresentationMatrix (SizedMatrix (ToMatSize n l c1 k1) (ToMatSize n l c2 k2) a) deriving (Show)
unDECrepresentationMatrix :: DECrepresentationMatrix n l c1 k1 c2 k2 a -> SizedMatrix (ToMatSize n l c1 k1) (ToMatSize n l c2 k2) a
unDECrepresentationMatrix = coerce

-- simplicial complex
-- [0,1,2], [1,3,2]
-- [0,1], [0,2], [1,2], [1,3], [2,3]
-- [0], ... , [3]
--  -> l = [4,5,2]
-- l !! k :: size of k-simplicial complex
type DifferentialForm n l c k a = DECrepresentationMatrix n l c k 'CEmpty 1 a
type VectorValuedDifferentialForm nEuc n l c k a = DECrepresentationMatrix n l c k 'CEmpty nEuc a
newtype PositionForm nEuc n l c k a = PositionForm (VectorValuedDifferentialForm nEuc n l c k a)

type ExteriorDerivative n l c k a = DECrepresentationMatrix n l c (SuccDeg n k) c k a
type HodgeStar n l c k a = DECrepresentationMatrix n l (DualMap c) (DualDeg n k) c k a
type InteriorProduct n l c k a = DifferentialForm n l (DualMap c) 1 a -> DECrepresentationMatrix n l c (PredDeg n k) c k a

-- example: 3d
-- Timelike: +---
-- Spacelike: -+++
data MetricSignature = Timelike | Spacelike
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable)
instance HasDefaultValue MetricSignature where
    defaultValue = Spacelike

type Codifferential n l c k a = DECrepresentationMatrix n l c (PredDeg n k) c k a
type Laplacian n l c k a = DECrepresentationMatrix n l c k c k a

type InclusionMap n l c a = DECrepresentationMatrix n l c (PredDeg n n) c (PredDeg n n) a

instance
    ( KnownNat n
    , SingI l
    , SingI c1
    , KnownNat k1
    , SingI c2
    , KnownNat k2
    , MSL.Numeric a
    ) =>
    Additive (DECrepresentationMatrix n l c1 k1 c2 k2 a)
    where
    DECrepresentationMatrix a + DECrepresentationMatrix b =
        case knownMatSizeDoublet @n @l @c1 @c2 @k1 @k2 of
            (Dict, Dict) -> DECrepresentationMatrix (a + b)
    zero = case knownMatSizeDoublet @n @l @c1 @c2 @k1 @k2 of
        (Dict, Dict) -> DECrepresentationMatrix zero

instance
    ( KnownNat n
    , SingI l
    , SingI c1
    , KnownNat k1
    , SingI c2
    , KnownNat k2
    , MSL.Numeric a
    , AdditiveGroup a
    ) =>
    AdditiveGroup (DECrepresentationMatrix n l c1 k1 c2 k2 a)
    where
    DECrepresentationMatrix a .-. DECrepresentationMatrix b = case knownMatSizeDoublet @n @l @c1 @c2 @k1 @k2 of
        (Dict, Dict) -> DECrepresentationMatrix (a .-. b)
    negation (DECrepresentationMatrix a) = case knownMatSizeDoublet @n @l @c1 @c2 @k1 @k2 of
        (Dict, Dict) -> DECrepresentationMatrix (negation a)

instance
    ( KnownNat n
    , SingI l
    , SingI c1
    , KnownNat k1
    , SingI c2
    , KnownNat k2
    , MSL.Numeric a
    , Multiplicative a
    ) =>
    Multiplicative (DECrepresentationMatrix n l c1 k1 c2 k2 a)
    where
    one = case knownMatSizeDoublet @n @l @c1 @c2 @k1 @k2 of
        (Dict, Dict) -> DECrepresentationMatrix one
    (DECrepresentationMatrix x) * (DECrepresentationMatrix y) = case knownMatSizeDoublet @n @l @c1 @c2 @k1 @k2 of
        (Dict, Dict) -> DECrepresentationMatrix $ x * y

instance
    ( MSL.Numeric a
    , KnownNat n
    , SingI l
    , SingI c1
    , SingI c2
    , SingI c3
    , KnownNat k1
    , KnownNat k2
    , KnownNat k3
    ) =>
    Mul (DECrepresentationMatrix n l c1 k1 c2 k2 a) (DECrepresentationMatrix n l c2 k2 c3 k3 a) (DECrepresentationMatrix n l c1 k1 c3 k3 a)
    where
    DECrepresentationMatrix a .@. DECrepresentationMatrix b =
        case knownMatSizeTriplet @n @l @c1 @c2 @c3 @k1 @k2 @k3 of (Dict, Dict, Dict) -> DECrepresentationMatrix (a .@. b)

instance
    ( MSL.Numeric a
    , KnownNat n
    , SingI l
    , SingI c1
    , KnownNat k1
    , SingI c2
    , KnownNat k2
    , Multiplicative a
    , VectorSpace a
    ) =>
    VectorSpace (DECrepresentationMatrix n l c1 k1 c2 k2 a)
    where
    type
        Scalar (DECrepresentationMatrix n l c1 k1 c2 k2 a) =
            ( Scalar
                (SizedMatrix (ToMatSize n l c1 k1) (ToMatSize n l c2 k2) a)
            )
    (*.) a (DECrepresentationMatrix x) = case knownMatSizeDoublet @n @l @c1 @c2 @k1 @k2 of
        (Dict, Dict) -> DECrepresentationMatrix (a *< x)

instance
    ( KnownNat n
    , SingI l
    , SingI c1
    , KnownNat k1
    , SingI c2
    , KnownNat k2
    , MSL.Numeric a
    , VST.Storable (RealField a)
    , Additive (RealField a)
    , NormSpace a
    , MSL.Numeric a
    , Ord (RealField a)
    , VectorSpace a
    , Multiplicative a
    , Transcendental (RealField a)
    ) =>
    NormSpace (DECrepresentationMatrix n l c1 k1 c2 k2 a)
    where
    type RealField (DECrepresentationMatrix n l c1 k1 c2 k2 a) = RealField a
    absPowSum t (DECrepresentationMatrix x) = case knownMatSizeDoublet @n @l @c1 @c2 @k1 @k2 of
        (Dict, Dict) -> absPowSum t x
    absMaxAll (DECrepresentationMatrix x) = case knownMatSizeDoublet @n @l @c1 @c2 @k1 @k2 of
        (Dict, Dict) -> absMaxAll x

instance
    ( MSL.Numeric a
    , NormSpace a
    , Multiplicative a
    , KnownNat n
    , SingI l
    , SingI c1
    , KnownNat k1
    , SingI c2
    , KnownNat k2
    ) =>
    InnerProductSpace (DECrepresentationMatrix n l c1 k1 c2 k2 a)
    where
    (DECrepresentationMatrix x) <.> (DECrepresentationMatrix y) = case knownMatSizeDoublet @n @l @c1 @c2 @k1 @k2 of
        (Dict, Dict) -> x <.> y
(<<.@.>>) :: forall n l c1 k1 c2 k2 c3 k3 a m. (KnownNat n, SingI l, SingI c1, SingI c2, SingI c3, KnownNat k1, KnownNat k2, KnownNat k3, Applicative m, MSL.Numeric a) => m (DECrepresentationMatrix n l c1 k1 c2 k2 a) -> m (DECrepresentationMatrix n l c2 k2 c3 k3 a) -> m (DECrepresentationMatrix n l c1 k1 c3 k3 a)
(<<.@.>>) = liftA2 (.@.)
infixl 7 <<.@.>>

deriving via (MyMatrix (DECrepresentationMatrix n l c1 k1 c2 k2 a)) instance ToVariableType (DECrepresentationMatrix n l c1 k1 c2 k2 a)