{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}

module Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Operators.Geometry where

-- interior product
-- Laplacian
-- codifferential

import Control.Algebra
import Data.Constraint
import qualified Data.Matrix.Static.LinearAlgebra as MSL
import Data.Singletons
import Data.Type.Equality
import qualified Data.Vector.Unboxed as VU
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Operators.Homology
import Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Proofs
import Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Types
import Formulative.Calculation.DiscreteExteriorCalculus.Geometry.Types
import Formulative.Calculation.DiscreteExteriorCalculus.Homology.Effect
import Formulative.Calculation.DiscreteExteriorCalculus.Homology.Operators
import Formulative.Calculation.Internal.Infix
import Formulative.Calculation.Internal.TypeLevelNatural
import GHC.TypeNats

-- TODO: undefinedの除去
-- TODO: 計量テンソルの符号を取り入れるか検討
hodgeStarMat :: m (HodgeStar n l c k a)
hodgeStarMat = undefined

hodgeStar ::
    forall n l c k c' k' a sig m.
    ( Algebra sig m
    , k <= n
    , KnownNat n
    , KnownNat k
    , KnownNat k'
    , SingI l
    , SingI c
    , SingI c'
    , MSL.Numeric a
    ) =>
    m (DECrepresentationMatrix n l c k c' k' a -> DECrepresentationMatrix n l (DualMap c) (DualDeg n k) c' k' a)
hodgeStar = case dualMapDict @c of
    Sub Dict -> do h <- hodgeStarMat; return (h .@.)

-- TODO: undefinedを取り除く
hodgeStarInv ::
    forall n l c k a sig m.
    ( Algebra sig m
    , k <= n
    , KnownNat n
    , KnownNat k
    , MSL.Numeric a
    , AdditiveGroup a
    , SingI l
    , SingI c
    ) =>
    m (DifferentialForm n l c k a -> DifferentialForm n l (DualMap c) (DualDeg n k) a)
hodgeStarInv = case dualMapDict @c of
    Sub Dict -> integralToSign (k * (n -1)) <$> hodgeStar
      where
        n = natVal (Proxy @n)
        k = natVal (Proxy @k)

-- TODO: 符号の確認
leftContraction :: forall n l c k1 k2 a sig m. (KnownNat k1, KnownNat k2, k2 <= n, k1 <= k2, Algebra sig m, SingI l, SingI c, KnownNat n, MSL.Numeric a, AdditiveGroup a) => m (DifferentialForm n l (DualMap c) k1 a -> DifferentialForm n l c k2 a -> DifferentialForm n l c (k2 - k1) a)
leftContraction = case dualityLaw @c of
    Refl -> case dualMapDict @c of
        Sub Dict -> do
            h1 <- hodgeStar
            h2 <- hodgeStar
            w <- wedge
            return $ \a b -> integralToSign (p * (q - 1)) $ h1 (a `w` h2 b)
  where
    p = natVal (Proxy :: Proxy k1)
    q = natVal (Proxy :: Proxy k2)

-- TODO: 符号の確認
codifferential :: forall n l c k a sig m. (1 <= k, k <= n, HasExteriorDerivative n l c k a sig m) => m (DifferentialForm n l c k a -> DifferentialForm n l c (PredDeg n k) a)
codifferential = case dualityLaw @c of
    Refl -> case dualMapDict @c of
        Sub Dict -> do
            h1 <- hodgeStarInv
            d <- exteriorDerivative
            h2 <- hodgeStar
            return (integralToSign k . h1 . d . h2)
  where
    k = natVal (Proxy @k)

-- TODO: 符号の確認
laplacian :: forall n l c k a sig m. (k <= n, HasExteriorDerivative n l c k a sig m) => m (DifferentialForm n l c k a -> DifferentialForm n l c k a)
laplacian = case (leqNat (Proxy :: Proxy 1) (Proxy :: Proxy k), leqNat (Proxy :: Proxy (k + 1)) (Proxy :: Proxy n)) of
    (Just Refl, Just Refl) -> negation <$> (codifferential |.| exteriorDerivative |.+.| exteriorDerivative |.| codifferential)
    (Just Refl, Nothing) -> negation <$> (exteriorDerivative |.| codifferential)
    (Nothing, Just Refl) -> negation <$> codifferential |.| exteriorDerivative
    (_, _) -> error "*** Exception: Laplacian"

lieDerivative :: forall n l c k a sig m. (k <= n, HasExteriorDerivative n l c k a sig m) => m (DifferentialForm n l (DualMap c) 1 a -> DifferentialForm n l c k a -> DifferentialForm n l c k a)
lieDerivative = case (leqNat (Proxy :: Proxy 1) (Proxy :: Proxy k), leqNat (Proxy :: Proxy (k + 1)) (Proxy :: Proxy n)) of
    (Just Refl, Just Refl) -> do
        i1 <- leftContraction
        i2 <- leftContraction
        d1 <- exteriorDerivative
        d2 <- exteriorDerivative
        return $ \v -> (i1 v . d1) .+. (d2 . i2 v)
    (Just Refl, Nothing) -> do
        i <- leftContraction
        d <- exteriorDerivative
        return $ \v -> d . i v
    (Nothing, Just Refl) -> do
        i <- leftContraction
        d <- exteriorDerivative
        return $ \v -> i v . d
    (_, _) -> error "*** Exception: Lie derivative"

grad ::
    forall n l c k a sig m.
    (HasExteriorDerivative n l c k a sig m) =>
    m (DifferentialForm n l c k a -> DifferentialForm n l c (SuccDeg n k) a)
grad = exteriorDerivative

-- TODO: 符号の確認
curl ::
    (Applicative m, (k + 1) <= n, SingI l, SingI c, MSL.Numeric a, KnownNat n, KnownNat k, Algebra sig m, SingI (DualMap c), Has (Connectivity n l) sig m, AdditiveGroup a, VU.Unbox a) =>
    m (DifferentialForm n l c k a -> DifferentialForm n l (DualMap c) (DualDeg n (k + 1)) a)
curl = hodgeStar |.| exteriorDerivative

div ::
    ( Applicative m
    , 1 <= k
    , (k + 1) <= n
    , KnownNat n
    , KnownNat k
    , SingI l
    , SingI c
    , Has (Connectivity n l) sig m
    , MSL.Numeric a
    , AdditiveGroup a
    , VU.Unbox a
    ) =>
    m (DifferentialForm n l c k a -> DifferentialForm n l c (k - 1) a)
div = negation <$> codifferential