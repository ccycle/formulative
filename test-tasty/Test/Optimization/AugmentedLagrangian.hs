{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Test.Optimization.AugmentedLagrangian where

import Control.Exception.Safe
import Data.Csv
import Dhall
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Optimization.Constrained.AugmentedLagrangian
import Formulative.Calculation.Optimization.Constrained.Types
import Formulative.Calculation.Optimization.LBFGS
import Formulative.Calculation.Optimization.LineSearch
import Formulative.Calculation.VectorSpace.Class
import Formulative.Preprocess.DefaultValue
import GHC.Generics
import Test.Tasty

data TestData = TestData Double Double
    deriving stock (Generic, Show, Eq)
    deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace, FromDhall, ToDhall, ToRecord)

f (TestData x y) = x + y
gradf (TestData x y) = TestData 1 1
g (TestData x y) = x ^ 2 + y ^ 2 - 1
gradG (TestData x y) = TestData (2 * x) (2 * y)
mu = 1.0
gradPenalty (LagrangeMultiplier lambda) x = lambda *. gradG x
lambda = 0.0
initialData = TestData (-1) (-1)
augmentedLagrangianParametersTest = defaultValue{penaltyCoefficient = PenaltyCoefficient mu, growthRate = GrowthRateForPenaltyCoefficient 1.1, maximumIterationNumber = IterationNumberForALM 1000}

augmentedLagrangianMethodTest :: (MonadThrow m) => TestData -> m (TestData, LagrangeMultiplier Double)
augmentedLagrangianMethodTest =
    augmentedLagrangianMethod
        defaultValue
        defaultValue
        defaultValue
        augmentedLagrangianParametersTest
        (ObjectiveFunction f)
        (GradObjectiveFunction gradf)
        (EqualityConstraint g)
        (LagrangeMultiplier lambda)
        (GradPenalty gradPenalty)
