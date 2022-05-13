{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.Coordinates.Dim3.Euclidean where

import Data.Csv
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.VectorSpace.Class
import Formulative.Postprocess.Export.Variable.Local
import GHC.Generics

data Euclidean3D a = Euclidean3D {x :: a, y :: a, z :: a}
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Additive, AdditiveGroup, VectorSpace)
    deriving anyclass (ToRecord, ToLazyField)
instance (Ord (RealField a), Additive (RealField a), NormSpace a) => NormSpace (Euclidean3D a)
instance (InnerProductSpace a, Additive (Scalar a)) => InnerProductSpace (Euclidean3D a)
