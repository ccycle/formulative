{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}

module Test.Optimization.AugmentedLagrangian where

import Control.Exception.Safe
import Data.Csv
import Dhall
import GHC.Generics
import HStructure.Calculation.Algebra.Arithmetic.Class
import HStructure.Calculation.Optimization.AugmentedLagrangian
import HStructure.Calculation.Optimization.LBFGS
import HStructure.Calculation.Optimization.LineSearch
import HStructure.Calculation.VectorSpace.Class
import Test.Tasty

data TestData = MkTestData Double Double
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace, FromDhall, ToDhall, ToRecord)

f (MkTestData x y) = x + y
gradf (MkTestData x y) = MkTestData 1 1
g (MkTestData x y) = x ^ 2 + y ^ 2 - 1
gradG (MkTestData x y) = MkTestData (2 * x) (2 * y)
mu = 1.0
gradPenalty (MkLagrangianMultiplier lambda) x = lambda *. gradG x
lambda = 0.0
initialData = MkTestData (-1) (-1)
augmentedLagrangianParametersTest = defaultAugmentedLagrangianParameters{penaltyCoefficient = MkPenaltyCoefficient mu, growthRateForPenaltyCoefficient = MkGrowthRateForPenaltyCoefficient 1.1, maxIterationAugmentedLagrangian = MkIterationNumberFormAugmentedLagrangian 1000}

augmentedLagrangianMethodTest :: (MonadThrow m) => TestData -> m (TestData, LagrangianMultiplier Double)
augmentedLagrangianMethodTest =
    augmentedLagrangianMethod
        defaultLineSearchParameters
        defaultConvergenceTestParameters
        defaultLBFGSParameters
        augmentedLagrangianParametersTest
        (MkObjectiveFunction f)
        (MkGradObjectiveFunction gradf)
        (MkEqualityConstraint g)
        (MkLagrangianMultiplier lambda)
        (MkGradPenalty gradPenalty)
