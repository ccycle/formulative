{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.Coordinates.Dim2.Parabolic where

import Data.Csv (ToRecord)
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Coordinates.Class
import Formulative.Calculation.Coordinates.Dim2.Euclidean
import Formulative.Calculation.VectorSpace.Class
import Formulative.Postprocess.Export.Variable.Local
import GHC.Generics
import Prelude hiding (fromInteger)

-- -oo < sigma < oo
-- 0 < tau < oo
data ParabolicCoord2d a = ParabolicCoord2d {sigma :: a, tau :: a}
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToRecord, ToLazyField)
instance (RealField a ~ Scalar a, Field (Scalar a), NormSpace a, Transcendental (Scalar a), Transcendental a) => HasCoordinateTransformation EuclideanCoord2d ParabolicCoord2d a where
    transformCoord EuclideanCoord2d{..} = ParabolicCoord2d (normalize LInfinity x .*. sqrt' (sqrt' (x .^ 2 .+. y .^ 2) .+. y)) (sqrt' (sqrt' (x .^ 2 .+. y .^ 2) .-. y))
instance (Field a) => HasCoordinateTransformation ParabolicCoord2d EuclideanCoord2d a where
    transformCoord ParabolicCoord2d{..} = EuclideanCoord2d (sigma .*. tau) ((tau .^ 2 .-. sigma .^ 2) ./. 2)
