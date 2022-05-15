{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.Coordinates.Dim3.Spherical where

import Data.Csv (ToRecord)
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Coordinates.Dim3.Euclidean
import Formulative.Calculation.VectorSpace.Class
import Formulative.Postprocess.Export.Variable.Class
import Formulative.Postprocess.Export.Variable.Local
import GHC.Generics

-- 0 < r
-- 0 < theta < pi
-- 0 < phi < 2*pi
data SphericalCoord a = SphericalCoord {r :: a, theta :: a, phi :: a}
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToRecord, ToLazyField)

-- TODO: undefinedの除去
instance Additive (SphericalCoord a) where
    zero = undefined
    SphericalCoord r1 theta1 phi1 .+. SphericalCoord r2 theta2 phi2 = undefined

instance AdditiveGroup a => AdditiveGroup (SphericalCoord a) where
    SphericalCoord r1 theta1 phi1 .-. SphericalCoord r2 theta2 phi2 = undefined

instance (AdditiveGroup a, Multiplicative a, VectorSpace a) => VectorSpace (SphericalCoord a) where
    type Scalar (SphericalCoord a) = Scalar a
    alpha *. SphericalCoord r1 theta1 phi1 = SphericalCoord (alpha *. r1) theta1 phi1

instance (Additive a, Multiplicative a, Floating a, RealFloat a, Field a) => CoordinateTransform3d SphericalCoord a where
    toEuclidean3d (SphericalCoord r1 theta1 phi1) = Euclidean3d (r1 .*. cos phi1 .*. sin theta1) (r1 .*. sin phi1 .*. sin theta1) (r1 .*. cos theta1)
    fromEuclidean3d (Euclidean3d x y z) = SphericalCoord (sqrt (x .^ 2 .+. y .^ 2 .+. z .^ 2)) (acos (z ./. sqrt (x .^ 2 .+. y .^ 2 .+. z .^ 2))) (atan2 y x)

deriving via (MyCoord SphericalCoord a) instance ToVariableType (SphericalCoord a)

instance (Ord (RealField a), Additive (RealField a), NormSpace a, Multiplicative a, Transcendental a, a ~ RealField a) => NormSpace (SphericalCoord a) where
    absPowSum p (SphericalCoord r1 theta1 phi1) = r1 .** p
    absMaxAll (SphericalCoord r1 theta1 phi1) = r1
    norm _ (SphericalCoord r1 theta1 phi1) = r1

instance (InnerProductSpace a, Additive (Scalar a), Multiplicative a) => InnerProductSpace (SphericalCoord a)
