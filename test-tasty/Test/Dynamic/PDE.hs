{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Test.Dynamic.PDE where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Effect.Sum
import Data.Csv (ToField, ToRecord)
import GHC.Generics
import GHC.Natural
import OptDEC.Calculation.Algebra.Arithmetic.Class
import OptDEC.Calculation.DifferentialEquation.Parameter
import OptDEC.Calculation.DiscreteExteriorCalculus.Algebra
import OptDEC.Calculation.DiscreteExteriorCalculus.Class
import OptDEC.Calculation.Internal.Class
import OptDEC.Calculation.Optimization.LineSearch
import OptDEC.Calculation.Optimization.Update
import OptDEC.Calculation.VectorSpace.Class
import OptDEC.Postprocess.Export.Class
import Test.Tasty

-- Cahn-Hilliard equation
data StateTest n l = MkTestData {position :: DifferentialForm n l Dual 0 Double, density :: DifferentialForm n l Dual 0 Double}
    deriving stock (Show, Generic)
    deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace)
    deriving anyclass (ToRecord, ToField)
