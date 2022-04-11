{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.Optimization.AugmentedLagrangian where

import Control.Exception.Safe
import CustomPrelude
import Data.Coerce
import Dhall
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Optimization.LBFGS
import Formulative.Calculation.Optimization.LineSearch
import Formulative.Calculation.VectorSpace.Class
import Formulative.Preprocess.DefaultValue
import Formulative.Preprocess.Exception
import RIO.Partial (pred)
import Prelude hiding (fromInteger)

newtype EqualityConstraint a b = MkEqualityConstraint (a -> b)
newtype LagrangianMultiplier a = MkLagrangianMultiplier a deriving (Show) -- a :: VectorSpace
unMkLagrangianMultiplier :: LagrangianMultiplier a -> a
unMkLagrangianMultiplier = coerce
newtype GradPenalty a b = MkGradPenalty (LagrangianMultiplier b -> a -> a) -- δ(<λ,g>)/δx

newtype PenaltyCoefficient a = MkPenaltyCoefficient a -- must be 0 /= a
    deriving stock (Generic, Show, Eq)
    deriving newtype (Num, Enum)
    deriving anyclass (FromDhall, ToDhall, Hashable)
instance (Fractional a) => DefaultValue (PenaltyCoefficient a) where
    defaultValue = MkPenaltyCoefficient 1e-12

newtype GrowthRateForPenaltyCoefficient a = MkGrowthRateForPenaltyCoefficient a -- must be 1 < a
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable)
instance (Fractional a) => DefaultValue (GrowthRateForPenaltyCoefficient a) where
    defaultValue = MkGrowthRateForPenaltyCoefficient 2.0
newtype TorelanceALM a = TorelanceALM a -- must be 0 /= a
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable)
instance (Fractional a) => DefaultValue (TorelanceALM a) where
    defaultValue = TorelanceALM 1e-8

newtype IterationNumberForALM = MkIterationNumberForALM Natural
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (Num, Enum)
    deriving anyclass (FromDhall, ToDhall, Hashable)
instance (Fractional a) => DefaultValue IterationNumberForALM where
    defaultValue = MkIterationNumberForALM 1000

data AugmentedLagrangianMethodParameters a = MkAugmentedLagrangianMethodParameters
    { penaltyCoefficient :: PenaltyCoefficient a
    , growthRate :: GrowthRateForPenaltyCoefficient a
    , torelance :: TorelanceALM a
    , maximumIterationNumber :: IterationNumberForALM
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable, DefaultValue)

newtype ConstrainedSystemParameter a = ConstrainedSystemParameter
    { augmentedLagrangianMethodParameters :: AugmentedLagrangianMethodParameters a
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable, DefaultValue)

{- | \[
 L=f+<\lambda,g>+(\frac{\mu}{2})<g,g>
 \]
 \[
 gradPenalty :: \delta(<\lambda,g>)
 \]
 \delta((\mu/2)<g,g>)=\delta(<\lambda,g>) | λ=μg
 内積のgradientは計算がかなり面倒
 使用者側が手計算経由で与えるほかない？
 gと\(\lambda\)の内部表現は同じ
-}
augmentedLagrangian (MkPenaltyCoefficient mu) (MkLagrangianMultiplier lambda) (MkObjectiveFunction f) (MkEqualityConstraint g) = MkObjectiveFunction $ \x -> f x .+. (mu ./. fromInteger 2 .*. g x <.> g x) .+. (lambda <.> g x)

augmentedLagrangianMethod
    lineSearchParam
    convergenceTestParam
    lbfgsParam
    (MkAugmentedLagrangianMethodParameters (MkPenaltyCoefficient mu) (MkGrowthRateForPenaltyCoefficient a) (TorelanceALM eps) i)
    (MkObjectiveFunction f)
    (MkGradObjectiveFunction gradF)
    (MkEqualityConstraint g)
    (MkLagrangianMultiplier lambda)
    (MkGradPenalty gradPenalty) =
        augmentedLagrangianMethod'
            lineSearchParam
            convergenceTestParam
            lbfgsParam
            (MkAugmentedLagrangianMethodParameters (MkPenaltyCoefficient mu) (MkGrowthRateForPenaltyCoefficient a) (TorelanceALM eps) i)
            (MkObjectiveFunction f)
            (MkGradObjectiveFunction gradF)
            (MkEqualityConstraint g)
            (MkLagrangianMultiplier lambda)
            (MkGradPenalty gradPenalty)
      where
        augmentedLagrangianMethod'
            lineSearchParam
            convergenceTestParam
            lbfgsParam
            (MkAugmentedLagrangianMethodParameters (MkPenaltyCoefficient mu) (MkGrowthRateForPenaltyCoefficient a) (TorelanceALM eps) i)
            (MkObjectiveFunction f)
            (MkGradObjectiveFunction gradF)
            (MkEqualityConstraint g)
            (MkLagrangianMultiplier lambda)
            (MkGradPenalty gradPenalty)
            x =
                do
                    x' <-
                        lbfgsMethod
                            lineSearchParam
                            convergenceTestParam
                            lbfgsParam
                            (augmentedLagrangian (MkPenaltyCoefficient mu) (MkLagrangianMultiplier lambda) (MkObjectiveFunction f) (MkEqualityConstraint g))
                            (MkGradObjectiveFunction $ \x -> gradF x .+. gradPenalty (MkLagrangianMultiplier lambda) x .+. gradPenalty (MkLagrangianMultiplier (mu *. g x)) x)
                            x
                    if i == 0
                        then throw $ ConvergenceException (norm (normType convergenceTestParam) $ gradF x)
                        else
                            if norm (normType convergenceTestParam) (g x') <= eps
                                then return (x', MkLagrangianMultiplier lambda)
                                else do
                                    augmentedLagrangianMethod'
                                        lineSearchParam
                                        convergenceTestParam
                                        lbfgsParam
                                        (MkAugmentedLagrangianMethodParameters (MkPenaltyCoefficient (a .*. mu)) (MkGrowthRateForPenaltyCoefficient a) (TorelanceALM eps) (pred i))
                                        (MkObjectiveFunction f)
                                        (MkGradObjectiveFunction gradF)
                                        (MkEqualityConstraint g)
                                        (MkLagrangianMultiplier (lambda .+. (mu *. g x')))
                                        (MkGradPenalty gradPenalty)
                                        x'