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
instance HasDefaultValue (NormType a) where
    defaultValue = LInfinity

-- absPow x = |x|^p
class (VectorSpace v) => NormSpace v where
    type RealField v
    absPow :: (RealField v) -> v -> RealField v
    default absPow :: (Generic v, GNormSpace (Rep v), GRealField (Rep v) ~ RealField v) => (RealField v) -> v -> RealField v
    type RealField v = GRealField (Rep v)
    absPow p v = gAbsPow p (from v)
    norm :: (Transcendental (RealField v)) => NormType (RealField v) -> v -> RealField v
    norm (Lp p) x = absPow (p) x .** reciprocal p
    norm LInfinity x = absPow 1 x

maxNorm x = norm LInfinity x
normL1 x = absPow (Lp 1) x
normL2 x = norm (Lp 2) x
normSquared x = absPow (Lp 2) x

binaryOpLp LInfinity = max
binaryOpLp lp = (.+.)

class GNormSpace f where
    type GRealField f
    gAbsPow :: (GRealField f) -> f v -> GRealField f
instance NormSpace s => GNormSpace (K1 i s) where
    type GRealField (K1 i s) = (RealField s)
    gAbsPow lp (K1 w) = absPow lp w
instance (GNormSpace a) => GNormSpace (M1 i c a) where
    gAbsPow lp (M1 w) = gAbsPow lp w
    type GRealField (M1 i c a) = GRealField a
instance (GNormSpace f, Ord (GRealField f), GNormSpace g, GRealField g ~ GRealField f, GScalar g ~ GScalar f, Additive (GRealField f)) => GNormSpace (f :*: g) where
    type GRealField (f :*: g) = GRealField f
    gAbsPow lp (x :*: y) = binaryOpLp (Lp lp) (gAbsPow lp x) (gAbsPow lp y)

instance (Num a, Transcendental a) => NormSpace (MyNum a) where
    type RealField (MyNum a) = a
    absPow p (MyNum a) = abs a .** p

-- absPow LInfinity (MyNum a) = abs a

-- deriving via (MyNum Int) instance NormSpace Int
-- deriving via (MyNum Integer) instance NormSpace Integer
-- deriving via (MyNum Natural) instance NormSpace Natural
deriving via (MyNum Double) instance NormSpace Double
deriving via (MyNum Float) instance NormSpace Float

instance (RealFloat a, Transcendental a) => NormSpace (MyComplex a) where
    type RealField (MyComplex a) = a
    absPow (p) (MyComplex a) = realPart (abs a) .** p

-- absPow (LInfinity) (MyComplex a) = realPart $ abs a

deriving via (MyComplex Double) instance NormSpace (Complex Double)
deriving via (MyComplex Float) instance NormSpace (Complex Float)

instance (Foldable m, Applicative m, Transcendental (RealField a), NormSpace a, Ord (RealField a), NormSpace (RealField a)) => NormSpace (MyFoldable m a) where
    type RealField (MyFoldable m a) = RealField a
    absPow (p) (MyFoldable a) = foldl' (binaryOpLp (Lp p)) zero (fmap (absPow (p)) a)
    norm (Lp p) (MyFoldable a) = absPow (p) (MyFoldable a) .** (1 ./. p)
    norm LInfinity (MyFoldable a) = foldl' (binaryOpLp LInfinity) zero (fmap (absPow 1) a)

deriving via (MyFoldable (VS.Vector n) a) instance (KnownNat n, Transcendental (RealField a), Multiplicative a, NormSpace a, Ord (RealField a), NormSpace (RealField a)) => NormSpace (VS.Vector n a)