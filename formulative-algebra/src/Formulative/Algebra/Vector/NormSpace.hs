{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Formulative.Algebra.Vector.NormSpace where

import Data.Foldable
import Data.Hashable
import qualified Data.Vector.Sized as VS
import Dhall
import Formulative.Algebra.Arithmetic
import Formulative.Algebra.Internal.FromPrelude
import Formulative.Algebra.Literal.FromInteger
import Formulative.Algebra.Vector.VectorSpace
import GHC.Generics
import GHC.TypeNats
import Prelude hiding (Num (..), fromInteger, recip, (**))
import qualified Prelude as P

-- 0 <= a
data NormType a = Lp a | LInfinity
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable)

class (VectorSpace v) => NormSpace v where
    type RealField v
    type RealField v = GRealField (Rep v)

    -- absPowSum x = |x1|^p + |x2|^p + ...
    absPowSum :: RealField v -> v -> RealField v
    default absPowSum :: (Generic v, GNormSpace (Rep v), GRealField (Rep v) ~ RealField v) => RealField v -> v -> RealField v
    absPowSum p v = gAbsPowSum p (from v)

    -- absMaxAll x = Max(|x1|, |x2|, ...)
    absMaxAll :: v -> RealField v
    default absMaxAll :: (Generic v, GNormSpace (Rep v), GRealField (Rep v) ~ RealField v) => v -> RealField v
    absMaxAll v = gAbsMaxAll (from v)

    norm :: NormType (RealField v) -> v -> RealField v
    default norm :: (Transcendental (RealField v)) => NormType (RealField v) -> v -> RealField v
    norm (Lp p) x = absPowSum p x ** recip p
    norm LInfinity x = absMaxAll x

maxNorm :: NormSpace v => v -> RealField v
maxNorm = norm LInfinity
normL1 :: (NormSpace v, FromInteger (RealField v)) => v -> RealField v
normL1 = norm (Lp 1)
normL2 :: (NormSpace v, FromInteger (RealField v)) => v -> RealField v
normL2 = norm (Lp 2)
normSquared :: (NormSpace v, FromInteger (RealField v)) => v -> RealField v
normSquared = absPowSum 2

binaryOpLp :: (Ord a1, Additive a1) => NormType a2 -> a1 -> a1 -> a1
binaryOpLp LInfinity = max
binaryOpLp (Lp _) = (+)

class GNormSpace f where
    type GRealField f
    gAbsPowSum :: GRealField f -> f v -> GRealField f
    gAbsMaxAll :: f v -> GRealField f
instance NormSpace s => GNormSpace (K1 i s) where
    type GRealField (K1 i s) = (RealField s)
    gAbsPowSum p (K1 w) = absPowSum p w
    gAbsMaxAll (K1 w) = absMaxAll w
instance (GNormSpace a) => GNormSpace (M1 i c a) where
    type GRealField (M1 i c a) = GRealField a
    gAbsPowSum lp (M1 w) = gAbsPowSum lp w
    gAbsMaxAll (M1 w) = gAbsMaxAll w
instance (GNormSpace f, Ord (GRealField f), GNormSpace g, GRealField g ~ GRealField f, GScalar g ~ GScalar f, Additive (GRealField f)) => GNormSpace (f :*: g) where
    type GRealField (f :*: g) = GRealField f
    gAbsPowSum p (x :*: y) = binaryOpLp (Lp p) (gAbsPowSum p x) (gAbsPowSum p y)
    gAbsMaxAll (x :*: y) = binaryOpLp LInfinity (gAbsMaxAll x) (gAbsMaxAll y)

instance (RealFloat a) => NormSpace (FromPrelude a) where
    type RealField (FromPrelude a) = a
    absPowSum p (FromPrelude a) = P.abs a P.** p
    absMaxAll (FromPrelude a) = P.abs a
    norm _ (FromPrelude a) = P.abs a

deriving via (FromPrelude Double) instance NormSpace Double
deriving via (FromPrelude Float) instance NormSpace Float

instance (Foldable m, Applicative m, Transcendental (RealField a), NormSpace a, Ord (RealField a), Multiplicative a, NormSpace (RealField a)) => NormSpace (FromPrelude1 m a) where
    type RealField (FromPrelude1 m a) = RealField a
    absPowSum p (FromPrelude1 a) = foldl' (binaryOpLp (Lp p)) zero (fmap (absPowSum p) a)
    absMaxAll (FromPrelude1 a) = foldl' (binaryOpLp LInfinity) zero (fmap absMaxAll a)

deriving via (FromPrelude1 (VS.Vector n) a) instance (KnownNat n, Transcendental (RealField a), Multiplicative a, NormSpace a, Ord (RealField a), NormSpace (RealField a)) => NormSpace (VS.Vector n a)
