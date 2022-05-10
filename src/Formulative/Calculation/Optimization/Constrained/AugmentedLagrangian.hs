{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.Optimization.Constrained.AugmentedLagrangian where

import Control.Exception.Safe
import Data.Coerce
import Data.Hashable
import Dhall
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Optimization.LBFGS
import Formulative.Calculation.Optimization.LineSearch
import Formulative.Calculation.VectorSpace.Class
import Formulative.Preprocess.DefaultValue
import Formulative.Preprocess.Exception
import Prelude hiding (fromInteger)

newtype EqualityConstraint a b = EqualityConstraint (a -> b)
newtype LagrangeMultiplier a = LagrangeMultiplier a deriving (Show) -- a :: VectorSpace
unLagrangeMultiplier :: LagrangeMultiplier a -> a
unLagrangeMultiplier = coerce
newtype GradPenalty a b = GradPenalty (LagrangeMultiplier b -> a -> a) -- δ(<λ,g>)/δx

newtype PenaltyCoefficient a = PenaltyCoefficient a -- must be 0 /= a
    deriving stock (Generic, Show, Eq)
    deriving newtype (Num, Enum, Fractional)
    deriving anyclass (FromDhall, ToDhall, Hashable)
instance (Fractional a) => HasDefaultValue (PenaltyCoefficient a) where
    defaultValue = PenaltyCoefficient 1e-12

newtype GrowthRateForPenaltyCoefficient a = GrowthRateForPenaltyCoefficient a -- must be 1 < a
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable)
instance (Fractional a) => HasDefaultValue (GrowthRateForPenaltyCoefficient a) where
    defaultValue = GrowthRateForPenaltyCoefficient 1.5
newtype TorelanceForConstrainedCondition a = TorelanceForConstrainedCondition a -- must be 0 /= a
    deriving stock (Generic, Show, Eq)
    deriving newtype (Num, Enum, Fractional)
    deriving anyclass (FromDhall, ToDhall, Hashable)
instance (Fractional a) => HasDefaultValue (TorelanceForConstrainedCondition a) where
    defaultValue = TorelanceForConstrainedCondition 1e-8

newtype IterationNumberForALM = IterationNumberForALM Natural
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (Num, Enum)
    deriving anyclass (FromDhall, ToDhall, Hashable)
instance (Fractional a) => HasDefaultValue IterationNumberForALM where
    defaultValue = IterationNumberForALM 1000

data AugmentedLagrangianMethodParameters a = AugmentedLagrangianMethodParameters
    { penaltyCoefficient :: PenaltyCoefficient a
    , growthRate :: GrowthRateForPenaltyCoefficient a
    , maximumIterationNumber :: IterationNumberForALM
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable, HasDefaultValue)

data ConstrainedSystemSetting a = ConstrainedSystemSetting
    { torelance :: TorelanceForConstrainedCondition a
    , augmentedLagrangian :: AugmentedLagrangianMethodParameters a
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable, HasDefaultValue)

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
augmentedLagrangianFunc (PenaltyCoefficient mu) (LagrangeMultiplier lambda) (ObjectiveFunction f) (EqualityConstraint g) = ObjectiveFunction $ \x -> f x .+. (mu ./. fromInteger 2 .*. g x <.> g x) .+. (lambda <.> g x)

augmentedLagrangianMethod
    lineSearchParam
    convergenceTestParam
    lbfgsParam
    (TorelanceForConstrainedCondition eps)
    (AugmentedLagrangianMethodParameters (PenaltyCoefficient mu) (GrowthRateForPenaltyCoefficient a) i)
    (ObjectiveFunction f)
    (GradObjectiveFunction gradF)
    (EqualityConstraint g)
    (LagrangeMultiplier lambda)
    (GradPenalty gradPenalty) =
        augmentedLagrangianMethod'
            lineSearchParam
            convergenceTestParam
            lbfgsParam
            (AugmentedLagrangianMethodParameters (PenaltyCoefficient mu) (GrowthRateForPenaltyCoefficient a) i)
            (ObjectiveFunction f)
            (GradObjectiveFunction gradF)
            (EqualityConstraint g)
            (LagrangeMultiplier lambda)
            (GradPenalty gradPenalty)
      where
        augmentedLagrangianMethod'
            lineSearchParam
            convergenceTestParam
            lbfgsParam
            (AugmentedLagrangianMethodParameters (PenaltyCoefficient mu) (GrowthRateForPenaltyCoefficient a) i)
            (ObjectiveFunction f)
            (GradObjectiveFunction gradF)
            (EqualityConstraint g)
            (LagrangeMultiplier lambda)
            (GradPenalty gradPenalty)
            x =
                do
                    x' <-
                        lbfgsMethod
                            lineSearchParam
                            convergenceTestParam
                            lbfgsParam
                            (augmentedLagrangianFunc (PenaltyCoefficient mu) (LagrangeMultiplier lambda) (ObjectiveFunction f) (EqualityConstraint g))
                            (GradObjectiveFunction $ \x -> gradF x .+. gradPenalty (LagrangeMultiplier lambda) x .+. gradPenalty (LagrangeMultiplier (mu *. g x)) x)
                            x
                    if i == 0
                        then throw $ ConvergenceException (norm (normType convergenceTestParam) $ gradF x)
                        else
                            if norm (normType convergenceTestParam) (g x') <= eps
                                then return (x', LagrangeMultiplier lambda)
                                else do
                                    augmentedLagrangianMethod'
                                        lineSearchParam
                                        convergenceTestParam
                                        lbfgsParam
                                        (AugmentedLagrangianMethodParameters (PenaltyCoefficient (a .*. mu)) (GrowthRateForPenaltyCoefficient a) (pred i))
                                        (ObjectiveFunction f)
                                        (GradObjectiveFunction gradF)
                                        (EqualityConstraint g)
                                        (LagrangeMultiplier (lambda .+. (mu *. g x')))
                                        (GradPenalty gradPenalty)
                                        x'