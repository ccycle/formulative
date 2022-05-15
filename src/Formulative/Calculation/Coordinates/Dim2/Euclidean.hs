{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.Coordinates.Dim2.Euclidean where

import Data.Csv
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.VectorSpace.Class
import Formulative.Postprocess.Export.Variable.Local
import GHC.Generics

data Euclidean2d a = Euclidean2d {x :: a, y :: a}
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Additive, AdditiveGroup, VectorSpace)
    deriving anyclass (ToRecord, ToLazyField)
instance (Ord (RealField a), Additive (RealField a), NormSpace a) => NormSpace (Euclidean2d a)
instance (InnerProductSpace a, Additive (Scalar a)) => InnerProductSpace (Euclidean2d a)

class CoordinateTransform2d f a where
    toEuclidean :: f a -> Euclidean2d a
    fromEuclidean :: Euclidean2d a -> f a