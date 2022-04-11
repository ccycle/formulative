{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Test.Optimization.AugmentedLagrangian where

import Control.Exception.Safe
import Data.Csv
import Dhall
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Optimization.AugmentedLagrangian
import Formulative.Calculation.Optimization.LBFGS
import Formulative.Calculation.Optimization.LineSearch
import Formulative.Calculation.VectorSpace.Class
import Formulative.Preprocess.DefaultValue
import GHC.Generics
import Test.Tasty

data TestData = MkTestData Double Double
    deriving stock (Generic, Show, Eq)
    deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace, FromDhall, ToDhall, ToRecord)

f (MkTestData x y) = x + y
gradf (MkTestData x y) = MkTestData 1 1
g (MkTestData x y) = x ^ 2 + y ^ 2 - 1
gradG (MkTestData x y) = MkTestData (2 * x) (2 * y)
mu = 1.0
gradPenalty (MkLagrangianMultiplier lambda) x = lambda *. gradG x
lambda = 0.0
initialData = MkTestData (-1) (-1)
augmentedLagrangianParametersTest = defaultValue{penaltyCoefficient = MkPenaltyCoefficient mu, growthRate = MkGrowthRateForPenaltyCoefficient 1.1, maximumIterationNumber = MkIterationNumberForALM 1000}

augmentedLagrangianMethodTest :: (MonadThrow m) => TestData -> m (TestData, LagrangianMultiplier Double)
augmentedLagrangianMethodTest =
    augmentedLagrangianMethod
        defaultValue
        defaultValue
        defaultValue
        augmentedLagrangianParametersTest
        (MkObjectiveFunction f)
        (MkGradObjectiveFunction gradf)
        (MkEqualityConstraint g)
        (MkLagrangianMultiplier lambda)
        (MkGradPenalty gradPenalty)
