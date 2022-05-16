{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.Coordinates.Dim2.Polar where

import Data.Csv
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Coordinates.Dim2.Euclidean
import Formulative.Calculation.VectorSpace.Class
import Formulative.Postprocess.Export.Variable.Local
import GHC.Generics
import Prelude hiding (fromInteger)

data PolarCoord a = PolarCoord {r :: a, theta :: a}
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToRecord, ToLazyField)

-- TODO: undefinedの除去
instance Additive a => Additive (PolarCoord a) where
    zero = PolarCoord zero zero
    PolarCoord r1 theta1 .+. PolarCoord r2 theta2 = undefined

instance AdditiveGroup a => AdditiveGroup (PolarCoord a) where
    PolarCoord r1 theta1 .-. PolarCoord r2 theta2 = undefined

instance (AdditiveGroup a, VectorSpace a) => VectorSpace (PolarCoord a) where
    alpha *. PolarCoord r1 theta1 = PolarCoord (alpha *. r1) theta1

instance (Multiplicative a, Floating a, Additive a, RealFloat a) => CoordinateTransform2d PolarCoord a where
    toEuclidean (PolarCoord r theta) = EuclideanCoord2d (r .*. cos theta) (r .*. sin theta)
    fromEuclidean (EuclideanCoord2d x y) = PolarCoord (sqrt (x .^ 2 .+. y .^ 2)) (atan2 y x)

instance (Ord (RealField a), Additive (RealField a), NormSpace a, Multiplicative a, Transcendental a, a ~ RealField a) => NormSpace (PolarCoord a) where
    absPowSum p (PolarCoord r theta) = r .** p
    absMaxAll (PolarCoord r theta) = r
    norm _ (PolarCoord r theta) = r

instance (InnerProductSpace a, Additive (Scalar a)) => InnerProductSpace (PolarCoord a)
