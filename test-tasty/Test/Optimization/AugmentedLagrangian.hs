{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}

module Test.Optimization.AugmentedLagrangian where

import Control.Exception.Safe
import GHC.Generics
import HStructure.Calculation.Algebra.Arithmetic.Class
import HStructure.Calculation.Optimization.AugmentedLagrangian
import HStructure.Calculation.Optimization.LBFGS
import HStructure.Calculation.Optimization.LineSearch
import HStructure.Calculation.VectorSpace.Class
import Test.Tasty

data TestData = MkTestData Double Double
    deriving stock (Eq, Show, Generic)
    deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace)

f (MkTestData x y) = x + y
gradf (MkTestData x y) = MkTestData 1 1
g (MkTestData x y) = 1 - x ^ 2 - y ^ 2
gradG (MkTestData x y) = MkTestData (- 2 * x) (- 2 * y)
mu = 1.0
gradPenalty (MkPenaltyCoefficient mu) (MkEqualityConstraint g) (MkLagrangianMultiplier lambda) x = (mu .*. g x) *. gradG x .+. lambda *. gradG x
lambda = 0.0
initialData = MkTestData (-1) (-1)
augmentedLagrangianParametersTest = defaultAugmentedLagrangianParameters{penaltyCoefficient = MkPenaltyCoefficient mu, growthRateForPenaltyCoefficient = MkGrowthRateForPenaltyCoefficient 1.1}

augmentedLagrangianMethodTest :: (MonadThrow m) => TestData -> m (TestData, LagrangianMultiplier Double, Residuals TestData)
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
