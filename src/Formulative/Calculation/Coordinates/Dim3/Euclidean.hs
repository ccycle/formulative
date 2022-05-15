{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.Coordinates.Dim3.Euclidean where

import Data.Csv (ToRecord)
import Data.Foldable (Foldable (foldl'))
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.VectorSpace.Class
import Formulative.Postprocess.Export.Variable.Class
import Formulative.Postprocess.Export.Variable.Local
import GHC.Generics

data Euclidean3d a = Euclidean3d {x :: a, y :: a, z :: a}
    deriving stock (Show, Eq, Generic, Functor)
    deriving anyclass (Additive, AdditiveGroup, Multiplicative, VectorSpace)
    deriving anyclass (ToRecord, ToLazyField)
instance (Ord (RealField a), Additive (RealField a), NormSpace a) => NormSpace (Euclidean3d a)

instance (InnerProductSpace a, Additive (Scalar a)) => InnerProductSpace (Euclidean3d a)

class CoordinateTransform3d f a where
    toEuclidean3d :: f a -> Euclidean3d a
    fromEuclidean3d :: Euclidean3d a -> f a

newtype MyCoord f a = MyCoord (f a)
instance ToVariableType (MyCoord f a) where
    toVariableType _ = ParticleType

deriving via (MyCoord Euclidean3d a) instance ToVariableType (Euclidean3d a)
