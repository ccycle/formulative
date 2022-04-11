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
import Formulative.Calculation.Optimization.Parameter
import Formulative.Calculation.VectorSpace.Class

type HasUpdateUnconstrainedSystem sig m b =
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
updateUnconstrainedSystem :: forall sig m b. HasUpdateUnconstrainedSystem sig m b => b -> m b
updateUnconstrainedSystem variable = do
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
            (MkObjectiveFunction objFunc)
            (MkGradObjectiveFunction gradObjFunc)
            variable

type HasUpdateConstrainedSystem sig m b =
    ( HasUpdateUnconstrainedSystem sig m b
    , HasEqualityConstraintM m b
    , HasGradPenaltyM m b
    , InnerProductSpace (EqualityConstraintType b)
    , Scalar b ~ Scalar (EqualityConstraintType b)
    , Scalar (EqualityConstraintType b) ~ RealField b
    , RealField (EqualityConstraintType b) ~ RealField b
    , NormSpace (EqualityConstraintType b)
    , Member (ConstrainedSystem (EqualityConstraintType b)) sig
    )
updateConstrainedSystem :: forall b sig m. HasUpdateConstrainedSystem sig m b => b -> m b
updateConstrainedSystem variable = do
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
                (MkObjectiveFunction objFunc)
                (MkGradObjectiveFunction gradObjFunc)
                (MkEqualityConstraint eqConst)
                lagrangianMultiplier
                (MkGradPenalty gradPenalty)
                variable
    putLagrangianMultiplier l
    return x
