{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.Optimization.Constrained.AugmentedLagrangian where

import Control.Exception.Safe
import Data.Coerce
import Data.Hashable
import Dhall
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Optimization.Constrained.Types
import Formulative.Calculation.Optimization.LBFGS
import Formulative.Calculation.Optimization.LineSearch
import Formulative.Calculation.VectorSpace.Class
import Formulative.Preprocess.DefaultValue
import Formulative.Preprocess.Exception
import Prelude hiding (fromInteger)

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
                                then return (VariablesConstrainedSystem x' (LagrangeMultiplier lambda))
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