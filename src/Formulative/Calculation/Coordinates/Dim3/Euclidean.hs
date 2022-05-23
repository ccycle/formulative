{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.Coordinates.Dim3.Euclidean where

import Data.Csv (FromRecord, ToRecord)
import Data.Foldable (Foldable (foldl'))
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.VectorSpace.Class
import Formulative.Postprocess.Export.Variable.Class
import Formulative.Postprocess.Export.Variable.Local
import GHC.Generics

data EuclideanCoord3d a = EuclideanCoord3d {x :: a, y :: a, z :: a}
    deriving stock (Show, Eq, Generic, Functor)
    deriving anyclass (Additive, AdditiveGroup, Multiplicative, VectorSpace)
    deriving anyclass (ToRecord, FromRecord, ToLazyField)
instance (Ord (RealField a), Additive (RealField a), NormSpace a, Transcendental (RealField a)) => NormSpace (EuclideanCoord3d a)

instance (InnerProductSpace a, Additive (Scalar a)) => InnerProductSpace (EuclideanCoord3d a)

newtype MyCoord f a = MyCoord (f a)
instance ToVariableType (MyCoord f a) where
    toVariableType _ = ParticleType

deriving via (MyCoord EuclideanCoord3d a) instance ToVariableType (EuclideanCoord3d a)
