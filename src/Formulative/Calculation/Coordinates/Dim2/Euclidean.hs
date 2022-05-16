{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.Coordinates.Dim2.Euclidean where

import Data.Csv (FromRecord, ToRecord)
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.VectorSpace.Class
import Formulative.Postprocess.Export.Variable.Local
import GHC.Generics

data EuclideanCoord2d a = EuclideanCoord2d {x :: a, y :: a}
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Additive, AdditiveGroup, VectorSpace)
    deriving anyclass (ToRecord, FromRecord, ToLazyField)
instance (Ord (RealField a), Additive (RealField a), NormSpace a) => NormSpace (EuclideanCoord2d a)
instance (InnerProductSpace a, Additive (Scalar a)) => InnerProductSpace (EuclideanCoord2d a)

class CoordinateTransform2d f a where
    toEuclidean :: f a -> EuclideanCoord2d a
    fromEuclidean :: EuclideanCoord2d a -> f a