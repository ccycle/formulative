{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.Coordinates.Dim2.Polar where

import Data.Csv (ToRecord)
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Coordinates.Class
import Formulative.Calculation.Coordinates.Dim2.Euclidean
import Formulative.Calculation.Internal.IfThenElse
import Formulative.Calculation.VectorSpace.Class
import Formulative.Postprocess.Export.Variable.Local
import GHC.Generics
import Prelude hiding (fromInteger)

data PolarCoord a = PolarCoord {r :: a, theta :: a}
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToRecord, ToLazyField)
instance (Transcendental a, Ord a) => HasCoordinateTransformation EuclideanCoord2d PolarCoord a where
    transformCoord EuclideanCoord2d{..} = PolarCoord (sqrt' (x .^ 2 .+. y .^ 2)) (if y < 0 then acos' (x ./. sqrt' (x .^ 2 .+. y .^ 2)) else negation $ acos' (x ./. sqrt' (x .^ 2 .+. y .^ 2)))
instance (Transcendental a) => HasCoordinateTransformation PolarCoord EuclideanCoord2d a where
    transformCoord PolarCoord{..} = EuclideanCoord2d (r .*. cos' theta) (r .*. sin' theta)
