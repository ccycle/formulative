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
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Proofs
import Formulative.Calculation.DiscreteExteriorCalculus.Geometry.Types
import Formulative.Calculation.VectorSpace.Class
import Formulative.Preprocess.DefaultValue
import GHC.TypeNats
import Unsafe.Coerce

-- import Formulative.Calculation.DiscreteExteriorCalculus.Proofs

-- -- https://blog.jle.im/entry/introduction-to-singletons-2.html

{- $( singletons
     [d|
         data CellType = Primal | Dual | CEmpty
             deriving (Show, Eq)
         |]
  )
-}

-- deriving stock instance Generic CellType
-- deriving anyclass instance FromDhall CellType
-- deriving anyclass instance ToDhall CellType
-- deriving anyclass instance Hashable CellType

-- sameCellType :: forall (c :: CellType) (c' :: CellType). (SingI c, SingI c') => Sing c -> Sing c' -> Maybe (c :~: c')
-- sameCellType singc singc'
--     | fromSing singc == fromSing singc' = Just (unsafeCoerce Refl)
--     | otherwise = Nothing

-- type family DualMap (c :: CellType) = r | r -> c where
--     DualMap 'Primal = 'Dual
--     DualMap 'Dual = 'Primal
--     DualMap 'CEmpty = 'CEmpty

-- dualMap :: CellType -> CellType
-- dualMap Primal = Dual
-- dualMap Dual = Primal
-- dualMap CEmpty = CEmpty

-- instance SingKind (DualMap c) where
--     type Demote (DualMap c) = CellType

--     fromSing :: Sing (xs :: SCellType k) -> List (Demote k)
--     fromSing = undefined

--     toSing :: List (Demote k) -> SomeSing (List k)
--     toSing = undefined -- ??? -- type instance HodgeDual n (DegreeOfForm (n + 1)) = Phi

-- primal n-form -> (d) -> primal (n+1)-form (empty) -> (star) -> dual (-1)-form (empty) -> (d) -> dual 0-form (empty) -> (star) -> primal n-form (empty)
-- primal 0-form -> (star) -> dual n-form -> (d) -> dual (n+1)-form (empty) -> (star) -> primal (-1)-form (empty) -> (d) -> primal 0-form (empty)
-- d :: dual empty-form -> dual 0-form
-- d :: primal empty-form -> primal empty-form
-- star :: dual empty-form -> primal empty-form
-- star :: primal empty-form -> dual empty-form

-- k-form -> k+1
-- emptyに対応させるため、数字を一個足す
-- 基底部は n=0 (empty)

-- type Nat = Nat
type DegreeOfForm = Nat

-- sizes of simplicial k-complex for each k
-- type [Nat] = [Nat]
-- type KnownHDims = SingI

-- -- from degree of form to matrix size
-- type family ToMatSize (n :: Nat) (l :: [Nat]) (c :: CellType) (k :: Nat) where
--     ToMatSize n l 'Primal k = l !! k
--     ToMatSize n l 'Dual k = l !! DualDeg n k
--     ToMatSize n l 'CEmpty k = k

-- type DualDeg (n :: Nat) (k :: Nat) = (n - k)

-- type SuccDeg (n :: Nat) (k :: Nat) = (k + 1)
-- type DualSuccDeg (n :: Nat) (k :: Nat) = DualDeg n (SuccDeg n k)

-- type PredDeg (n :: Nat) (k :: Nat) = (k - 1)
-- type DualPredDeg (n :: Nat) (k :: Nat) = DualDeg n (PredDeg n k)

-- type SizedMatrix p1 p2 a = MSL.SparseMatrix p1 p2 a -- 実装に使う型を選択

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

-- singletons
--     [d|
--         data OperatorType
--             = DifferentialFormType
--             | ExteriorDerivativeType
--             | HodgeStarType
--             | InteriorProductType
--             | LieDerivativeType
--             | CodifferentialType
--             | LaplacianType
--             | InclusionMapType
--             deriving (Show, Eq)
--         |]

-- -- size : p2*p1 のうちp2の部分を取得
-- type family ToMatSizeFromType n l c k t where
--     ToMatSizeFromType n l c k DifferentialFormType = (ToMatSize n l c k)
--     ToMatSizeFromType n l c k ExteriorDerivativeType = (ToMatSize n l c (SuccDeg n k))
--     ToMatSizeFromType n l c k InteriorProductType = (ToMatSize n l c (PredDeg n n))
--     ToMatSizeFromType n l c k LieDerivativeType = (ToMatSize n l c k)
--     ToMatSizeFromType n l c _ InclusionMapType = (ToMatSize n l c (PredDeg n n))
--     ToMatSizeFromType n l c k HodgeStarType = (ToMatSize n l (DualMap c) (DualDeg n k))
--     ToMatSizeFromType n l c k CodifferentialType = (ToMatSize n l c (PredDeg n k))
--     ToMatSizeFromType n l c k LaplacianType = (ToMatSize n l c k)

-- TODO: 削除してDictに置き換え
-- type KnownMatSize n l c k t = (KnownNat (ToMatSizeFromType n l c k t), KnownNat (ToMatSize n l c k))

-- deriving instance
-- instance (KnownNat (ToMatSize n l c1 k1), KnownNat (ToMatSize n l c2 k2), MSL.Numeric a) => Additive (DECrepresentationMatrix n l c1 k1 c2 k2 a) where
--     DECrepresentationMatrix a .+. DECrepresentationMatrix b = DECrepresentationMatrix (a .+. b)
--     zero = DECrepresentationMatrix zero

-- instance (KnownNat (ToMatSize n l c1 k1), KnownNat (ToMatSize n l c2 k2), MSL.Numeric a, AdditiveGroup a) => AdditiveGroup (DECrepresentationMatrix n l c1 k1 c2 k2 a) where
--     DECrepresentationMatrix a .-. DECrepresentationMatrix b = DECrepresentationMatrix (a .-. b)
--     negation (DECrepresentationMatrix a) = DECrepresentationMatrix (negation a)

-- instance (KnownNat n, KnownNat (ToMatSize n l c1 k1), KnownNat (ToMatSize n l c2 k2), MSL.Numeric a, Multiplicative a) => Multiplicative (DECrepresentationMatrix n l c1 k1 c2 k2 a) where
--     one = DECrepresentationMatrix one
--     (DECrepresentationMatrix x) .*. (DECrepresentationMatrix y) = DECrepresentationMatrix $ x .*. y

-- -- -- degreeの計算はphantom type上でのみ行う
-- -- instance (MSL.Numeric a, KnownNat k1, KnownNat k2, KnownNat k3, KnownNat (ToMatSize n l c1 k1), KnownNat (ToMatSize n l c2 k2), KnownNat (ToMatSize n l c3 k3)) => Mul (DECrepresentationMatrix n l c1 k1 c2 k2 a) (DECrepresentationMatrix n l c2 k2 c3 k3 a) (DECrepresentationMatrix n l c1 k1 c3 k3 a) where
-- --     DECrepresentationMatrix a .@. DECrepresentationMatrix b =
-- --         let proxyn = Proxy :: Proxy n
-- --             proxyl = sing :: Sing l
-- --             proxyc1 = sing :: SCellType c1
-- --             proxyc2 = sing :: SCellType c2
-- --             proxyc3 = sing :: SCellType c3
-- --             proxyk1 = Proxy :: Proxy k1
-- --             proxyk2 = Proxy :: Proxy k2
-- --             proxyk3 = Proxy :: Proxy k3
-- --          in case (toMatSizeDict proxyn proxyl proxyc1 proxyk1) of Dict -> DECrepresentationMatrix (a .@. b)

-- instance (KnownNat (ToMatSize n l c1 k1), KnownNat (ToMatSize n l c2 k2), MSL.Numeric a, Multiplicative a, VectorSpace a) => VectorSpace (DECrepresentationMatrix n l c1 k1 c2 k2 a) where
--     type
--         Scalar (DECrepresentationMatrix n l c1 k1 c2 k2 a) =
--             ( Scalar
--                 (SizedMatrix (ToMatSize n l c1 k1) (ToMatSize n l c2 k2) a)
--             )
--     (*.) a (DECrepresentationMatrix x) = DECrepresentationMatrix (a *. x)

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
    DECrepresentationMatrix a .+. DECrepresentationMatrix b =
        case knownMatSizeDoublet @n @l @c1 @c2 @k1 @k2 of
            (Dict, Dict) -> DECrepresentationMatrix (a .+. b)
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
    (DECrepresentationMatrix x) .*. (DECrepresentationMatrix y) = case knownMatSizeDoublet @n @l @c1 @c2 @k1 @k2 of
        (Dict, Dict) -> DECrepresentationMatrix $ x .*. y

-- -- degreeの計算はphantom type上でのみ行う
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
        let proxyn = Proxy :: Proxy n
            proxyl = sing :: Sing l
            proxyc1 = sing :: SCellType c1
            proxyc2 = sing :: SCellType c2
            proxyc3 = sing :: SCellType c3
            proxyk1 = Proxy :: Proxy k1
            proxyk2 = Proxy :: Proxy k2
            proxyk3 = Proxy :: Proxy k3
         in case knownMatSizeTriplet @n @l @c1 @c2 @c3 @k1 @k2 @k3 of (Dict, Dict, Dict) -> DECrepresentationMatrix (a .@. b)

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
        (Dict, Dict) -> DECrepresentationMatrix (a *. x)

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
    norm t (DECrepresentationMatrix x) = case knownMatSizeDoublet @n @l @c1 @c2 @k1 @k2 of
        (Dict, Dict) -> norm t x

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