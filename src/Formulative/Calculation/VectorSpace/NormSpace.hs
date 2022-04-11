{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.VectorSpace.NormSpace where

import Data.Complex
import Data.Foldable
import Data.Hashable
import qualified Data.Vector.Sized as VS
import Dhall
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Internal.Types
import Formulative.Calculation.VectorSpace.VectorSpace
import Formulative.Preprocess.DefaultValue
import GHC.Generics
import GHC.TypeNats
import Prelude hiding (fromInteger)

-- 0 <= a
data NormType a = Lp a | LInfinity
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable)
instance DefaultValue (NormType a) where
    defaultValue = LInfinity

class (VectorSpace v) => NormSpace v where
    type RealField v
    norm :: NormType (RealField v) -> v -> RealField v
    default norm :: (Generic v, GNormSpace (Rep v), GRealField (Rep v) ~ RealField v) => NormType (RealField v) -> v -> RealField v
    type RealField v = GRealField (Rep v)
    norm lp v = gNorm lp (from v)

maxNorm :: (NormSpace v) => v -> RealField v
maxNorm = norm LInfinity
normL1 :: (NormSpace v, Ring (RealField v)) => v -> RealField v
normL1 = norm (Lp (fromInteger 1))
normL2 :: (NormSpace v, Ring (RealField v)) => v -> RealField v
normL2 = norm (Lp (fromInteger 2))
normSquared :: (NormSpace v, Ring (RealField v)) => v -> RealField v
normSquared = (.^ 2) . norm (Lp (fromInteger 2))

binaryOpLp LInfinity = max
binaryOpLp lp = (.+.)

class GNormSpace f where
    type GRealField f
    gNorm :: NormType (GRealField f) -> f v -> GRealField f
instance NormSpace s => GNormSpace (K1 i s) where
    type GRealField (K1 i s) = (RealField s)
    gNorm lp (K1 w) = norm lp w
instance (GNormSpace a) => GNormSpace (M1 i c a) where
    gNorm lp (M1 w) = gNorm lp w
    type GRealField (M1 i c a) = GRealField a
instance (GNormSpace f, Ord (GRealField f), GNormSpace g, GRealField g ~ GRealField f, GScalar g ~ GScalar f, Additive (GRealField f)) => GNormSpace (f :*: g) where
    type GRealField (f :*: g) = GRealField f
    gNorm lp (x :*: y) = binaryOpLp lp (gNorm lp x) (gNorm lp y)

instance (Num a) => NormSpace (MyNum a) where
    type RealField (MyNum a) = a
    norm _ (MkMyNum a) = abs a

deriving via (MyNum Int) instance NormSpace Int
deriving via (MyNum Integer) instance NormSpace Integer
deriving via (MyNum Natural) instance NormSpace Natural
deriving via (MyNum Double) instance NormSpace Double
deriving via (MyNum Float) instance NormSpace Float

instance (RealFloat a) => NormSpace (MyComplex a) where
    type RealField (MyComplex a) = a
    norm _ (MkMyComplex a) = realPart $ abs a

deriving via (MyComplex Double) instance NormSpace (Complex Double)
deriving via (MyComplex Float) instance NormSpace (Complex Float)

instance (Foldable m, Applicative m, Transcendental (RealField a), NormSpace a, Ord (RealField a), NormSpace (RealField a)) => NormSpace (MyFoldable m a) where
    type RealField (MyFoldable m a) = RealField a
    norm LInfinity (MkMyFoldable a) = foldl' (binaryOpLp LInfinity) zero (fmap (norm LInfinity) a)
    norm (Lp p) (MkMyFoldable a) = (.**. reciprocal p) $ foldl' (binaryOpLp (Lp p)) zero (fmap ((.**. p) . norm (Lp p)) a)

deriving via (MyFoldable (VS.Vector n) a) instance (KnownNat n, Transcendental (RealField a), Multiplicative a, NormSpace a, Ord (RealField a), NormSpace (RealField a)) => NormSpace (VS.Vector n a)