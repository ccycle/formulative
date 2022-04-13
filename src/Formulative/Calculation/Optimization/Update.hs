module Formulative.Calculation.Optimization.Update where

import Control.Algebra
import Control.Effect.Sum
import Control.Effect.Throw
import Control.Exception.Safe
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Internal.Class
import Formulative.Calculation.Optimization.AugmentedLagrangian
import Formulative.Calculation.Optimization.Constrained.Effect
import Formulative.Calculation.Optimization.Effect
import Formulative.Calculation.Optimization.LBFGS
import Formulative.Calculation.Optimization.LineSearch
import Formulative.Calculation.VectorSpace.Class

type HasUpdateWithOptimization sig m b =
    ( Algebra sig m
    , NormSpace b
    , InnerProductSpace b
    , Absolute (Scalar b)
    , Field (Scalar b)
    , Show (RealField b)
    , Typeable (RealField b)
    , Ord (RealField b)
    , RealField b ~ Scalar b
    , Member (Throw SomeException) sig
    , Member (Optimization (RealField b)) sig
    , HasGradObjectiveFunctionM m b
    , HasObjectiveFunctionM m b
    )
updateWithOptimization :: forall sig m b. HasUpdateWithOptimization sig m b => b -> m b
updateWithOptimization variable = do
    objFunc <- getObjectiveFunctionM
    gradObjFunc <- getGradientOfObjectiveFunctionM
    lbfgsParam <- askLBFGSParameters @(RealField b)
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

type HasUpdateWithConstrainedOptimization sig m b =
    ( HasUpdateWithOptimization sig m b
    , HasEqualityConstraintM m b
    , HasGradPenaltyM m b
    , InnerProductSpace (EqualityConstraintType b)
    , Scalar b ~ Scalar (EqualityConstraintType b)
    , Scalar (EqualityConstraintType b) ~ RealField b
    , RealField (EqualityConstraintType b) ~ RealField b
    , NormSpace (EqualityConstraintType b)
    , Member (ConstrainedSystem (EqualityConstraintType b)) sig
    )
updateWithConstrainedOptimization :: forall b sig m. HasUpdateWithConstrainedOptimization sig m b => b -> m b
updateWithConstrainedOptimization variable = do
    objFunc <- getObjectiveFunctionM
    gradObjFunc <- getGradientOfObjectiveFunctionM
    eqConst <- getEqualityConstraintM
    gradPenalty <- getGradPenaltyM
    lbfgsParam <- askLBFGSParameters @(Scalar b)
    lineSearchParam <- askLineSearchParameters @(Scalar b)
    aParam <- askAugmentedLagrangianParameter @(EqualityConstraintType b)
    convergenceParam <- askConvergenceTestParameters @(Scalar b)
    lagrangianMultiplier <- getLagrangianMultiplier
    (x, l) <-
        liftEither $
            augmentedLagrangianMethod
                lineSearchParam
                convergenceParam
                lbfgsParam
                aParam
                (ObjectiveFunction objFunc)
                (GradObjectiveFunction gradObjFunc)
                (EqualityConstraint eqConst)
                lagrangianMultiplier
                (GradPenalty gradPenalty)
                variable
    putLagrangianMultiplier l
    return x
