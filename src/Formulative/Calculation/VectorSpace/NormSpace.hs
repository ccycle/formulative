{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}

module Formulative.Calculation.VectorSpace.NormSpace where

import Data.Complex
import Data.Foldable
import Data.Hashable
import qualified Data.Vector.Sized as VS
import Dhall
import Formulative.Calculation.Algebra.Arithmetic
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
instance HasDefaultValue (NormType a) where
    defaultValue = LInfinity

class (VectorSpace v) => NormSpace v where
    type RealField v
    type RealField v = GRealField (Rep v)

    -- absPowSum x = |x1|^p + |x2|^p + ...
    absPowSum :: RealField v -> v -> RealField v
    default absPowSum :: (Generic v, GNormSpace (Rep v), GRealField (Rep v) ~ RealField v) => RealField v -> v -> RealField v
    absPowSum p v = gAbsPowSum p (from v)

    absMaxAll :: v -> RealField v
    default absMaxAll :: (Generic v, GNormSpace (Rep v), GRealField (Rep v) ~ RealField v) => v -> RealField v
    absMaxAll v = gAbsMaxAll (from v)

    norm :: (NormSpace v) => NormType (RealField v) -> v -> RealField v
    default norm :: (NormSpace v, Transcendental (RealField v)) => NormType (RealField v) -> v -> RealField v
    norm (Lp p) x = absPowSum p x .** reciprocal p
    norm LInfinity x = absMaxAll x

maxNorm x = norm LInfinity x
normL1 x = norm (Lp 1) x
normL2 x = norm (Lp 2) x
normSquared x = absPowSum 2 x

binaryOpLp LInfinity = max
binaryOpLp (Lp p) = (.+.)

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

instance (Integral a) => NormSpace (MyNumeric a) where
    type RealField (MyNumeric a) = a
    absPowSum p (MyNumeric a) = abs a ^ p
    absMaxAll (MyNumeric a) = abs a
    norm _ (MyNumeric a) = abs a

deriving via (MyNumeric Int) instance NormSpace Int
deriving via (MyNumeric Integer) instance NormSpace Integer
deriving via (MyNumeric Natural) instance NormSpace Natural

instance (Foldable m, Applicative m, Transcendental (RealField a), NormSpace a, Ord (RealField a), NormSpace (RealField a)) => NormSpace (MyFoldable m a) where
    type RealField (MyFoldable m a) = RealField a
    absPowSum p (MyFoldable a) = foldl' (binaryOpLp (Lp p)) zero (fmap (absPowSum p) a)
    absMaxAll (MyFoldable a) = foldl' (binaryOpLp LInfinity) zero (fmap absMaxAll a)

deriving via (MyFoldable (VS.Vector n) a) instance (KnownNat n, Transcendental (RealField a), Multiplicative a, NormSpace a, Ord (RealField a), NormSpace (RealField a)) => NormSpace (VS.Vector n a)

-- Double -> Complex Double, ..
class ConvertNumeric a b where
    convertNumeric :: a -> b
    default convertNumeric :: (a ~ b) => a -> b
    convertNumeric = id
