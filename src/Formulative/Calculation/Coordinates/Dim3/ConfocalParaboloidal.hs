{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.Coordinates.Dim3.ConfocalParaboloidal where

import qualified Data.ByteString.Lazy as BSL
import Data.Csv
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Coordinates.Dim3.Euclidean
import Formulative.Calculation.VectorSpace.Class
import Formulative.Postprocess.Export.CSV
import Formulative.Postprocess.Export.Variable.Class
import Formulative.Postprocess.Export.Variable.Local
import GHC.Generics
import Path

-- 0<a<b
data ParaboloidParam c = ParaboloidParam {a :: c, b :: c}
    deriving stock (Show, Eq, Generic)
data ConfocalParaboloidCoord c = ConfocalParaboloidCoord {lambda :: c, mu :: c, nu :: c}
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToRecord, ToLazyField)
instance ToVariableType (ConfocalParaboloidCoord a) where
    toVariableType _ = ParticleType

toXSquared ParaboloidParam{..} ConfocalParaboloidCoord{..} = (a .^ 2 .-. lambda) .*. (mu .-. a .^ 2) .*. (nu .-. a .^ 2) ./. (b .^ 2 .-. a .^ 2)
toYSquared ParaboloidParam{..} ConfocalParaboloidCoord{..} = (b .^ 2 .-. lambda) .*. (b .^ 2 .-. mu) .*. (nu .-. b .^ 2) ./. (b .^ 2 .-. a .^ 2)
toZ ParaboloidParam{..} ConfocalParaboloidCoord{..} = lambda .+. mu .+. nu .-. a .^ 2 .-. b .^ 2

-- f c x = [toXSquared c x, toYSquared c x, toZ c x]
-- c = ParaboloidParam 1.0 2.0

-- TODO: undefinedの除去
instance Additive a => Additive (ConfocalParaboloidCoord a) where
    zero = undefined
    ConfocalParaboloidCoord lambda1 mu1 nu1 .+. ConfocalParaboloidCoord lambda2 mu2 nu2 = undefined

instance AdditiveGroup a => AdditiveGroup (ConfocalParaboloidCoord a) where
    ConfocalParaboloidCoord lambda1 mu1 nu1 .-. ConfocalParaboloidCoord lambda2 mu2 nu2 = undefined

instance AdditiveGroup a => VectorSpace (ConfocalParaboloidCoord a) where
    alpha *. ConfocalParaboloidCoord lambda1 mu1 nu1 = undefined

instance (Multiplicative a, Floating a, Additive a, RealFloat a) => CoordinateTransform3d ConfocalParaboloidCoord a where
    toEuclidean3d (ConfocalParaboloidCoord lambda1 mu1 nu1) = undefined
    fromEuclidean3d (EuclideanCoord3d x y z) = undefined

instance (Ord (RealField a), Additive (RealField a), NormSpace a, Multiplicative a, Transcendental a, a ~ RealField a) => NormSpace (ConfocalParaboloidCoord a) where
    absPowSum _ (ConfocalParaboloidCoord lambda1 mu1 nu1) = undefined
    absMaxAll (ConfocalParaboloidCoord lambda1 mu1 nu1) = undefined

instance (InnerProductSpace a, Additive (Scalar a)) => InnerProductSpace (ConfocalParaboloidCoord a)
