module Formulative.Calculation.Optimization.Update where

import Control.Algebra
import Control.Effect.Sum
import Control.Effect.Throw
import Control.Exception.Safe
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Optimization.Class
import Formulative.Calculation.Optimization.Constrained.AugmentedLagrangian
import Formulative.Calculation.Optimization.Constrained.Class
import Formulative.Calculation.Optimization.Constrained.Effect
import Formulative.Calculation.Optimization.Constrained.Types
import Formulative.Calculation.Optimization.Effect
import Formulative.Calculation.Optimization.LBFGS
import Formulative.Calculation.Optimization.LineSearch
import Formulative.Calculation.VectorSpace.Class

type HasUpdateWithOptimization sig m a =
    ( Algebra sig m
    , NormSpace a
    , InnerProductSpace a
    , Absolute (Scalar a)
    , Field (Scalar a)
    , Show (RealField a)
    , Typeable (RealField a)
    , Transcendental (Scalar a)
    , Ord (RealField a)
    , RealField a ~ Scalar a
    , Member (Throw SomeException) sig
    , Member (Optimization (RealField a)) sig
    , HasGradObjectiveFunctionM m a
    , HasObjectiveFunctionM m a
    )
updateWithOptimization :: forall sig m a. (HasUpdateWithOptimization sig m a) => a -> m a
updateWithOptimization variable = do
    objFunc <- getObjectiveFunctionM
    gradObjFunc <- getGradientOfObjectiveFunctionM
    lbfgsParam <- askLBFGSParameters @(RealField a)
    lineSearchParam <- askLineSearchParameters
    convergenceParam <- askConvergenceTestParameters
    liftEither $
        lbfgsMethod
            lineSearchParam
            convergenceParam
            lbfgsParam
            (ObjectiveFunction objFunc)
            (GradObjectiveFunction gradObjFunc)
            variable

type HasUpdateWithConstrainedOptimization sig m a =
    ( HasUpdateWithOptimization sig m a
    , HasEqualityConstraintM m a
    , HasGradPenaltyM m a
    , InnerProductSpace (EqualityConstraintType a)
    , Scalar a ~ Scalar (EqualityConstraintType a)
    , Scalar (EqualityConstraintType a) ~ RealField a
    , RealField (EqualityConstraintType a) ~ RealField a
    , NormSpace (EqualityConstraintType a)
    , Member (ConstrainedSystem (EqualityConstraintType a)) sig
    )
updateWithConstrainedOptimization :: forall b sig m. (HasUpdateWithConstrainedOptimization sig m b, Transcendental (Scalar (EqualityConstraintType b))) => b -> m b
updateWithConstrainedOptimization variable = do
    objFunc <- getObjectiveFunctionM
    gradObjFunc <- getGradientOfObjectiveFunctionM
    eqConst <- getEqualityConstraintM
    gradPenalty <- getGradPenaltyM
    lbfgsParam <- askLBFGSParameters @(Scalar b)
    lineSearchParam <- askLineSearchParameters @(Scalar b)
    torelanceM <- askTorelance @(EqualityConstraintType b)
    aParam <- askAugmentedLagrangianParameter @(EqualityConstraintType b)
    convergenceParam <- askConvergenceTestParameters @(Scalar b)
    lagrangeMultiplier <- getLagrangeMultiplier
    (VariablesConstrainedSystem x l) <-
        liftEither $
            augmentedLagrangianMethod
                lineSearchParam
                convergenceParam
                lbfgsParam
                torelanceM
                aParam
                (ObjectiveFunction objFunc)
                (GradObjectiveFunction gradObjFunc)
                (EqualityConstraint eqConst)
                (LagrangeMultiplier lagrangeMultiplier)
                (GradPenalty gradPenalty)
                variable
    putLagrangeMultiplier l
    return x