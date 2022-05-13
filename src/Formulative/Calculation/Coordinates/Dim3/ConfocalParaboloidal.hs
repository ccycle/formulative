{-# LANGUAGE DeriveAnyClass #-}

module Formulative.Calculation.Coordinates.Dim3.ConfocalParaboloidal where

import qualified Data.ByteString.Lazy as BSL
import Data.Csv
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.VectorSpace.Class
import Formulative.Postprocess.Export.CSV
import Formulative.Postprocess.Export.Variable.Class
import Formulative.Postprocess.Export.Variable.Local
import GHC.Generics
import Path

-- 0<a<b
data ParaboloidParam c = ParaboloidParam {a :: c, b :: c}
    deriving stock (Show, Eq, Generic)
data ParaboloidCoord c = ParaboloidCoord {lambda :: c, mu :: c, nu :: c}
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Additive, AdditiveGroup, VectorSpace)
    deriving anyclass (ToRecord, ToLazyField)
instance ToVariableType ToVariableType where
    toVariableType _ = ParticleType

toXSquared ParaboloidParam{..} ParaboloidCoord{..} = (a .^ 2 .-. lambda) .*. (mu .-. a .^ 2) .*. (nu .-. a .^ 2) ./. (b .^ 2 .-. a .^ 2)
toYSquared ParaboloidParam{..} ParaboloidCoord{..} = (b .^ 2 .-. lambda) .*. (b .^ 2 .-. mu) .*. (nu .-. b .^ 2) ./. (b .^ 2 .-. a .^ 2)
toZ ParaboloidParam{..} ParaboloidCoord{..} = lambda .+. mu .+. nu .-. a .^ 2 .-. b .^ 2

f c x = [toXSquared c x, toYSquared c x, toZ c x]
c = ParaboloidParam 1.0 2.0