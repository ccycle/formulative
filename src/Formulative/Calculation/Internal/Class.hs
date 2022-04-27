{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Formulative.Calculation.Internal.Class where

import Control.Algebra
import Data.Csv (DefaultOrdered, ToNamedRecord, ToRecord)
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Internal.Variable.Effect
import Formulative.Calculation.Optimization.Constrained.AugmentedLagrangian
import Formulative.Calculation.VectorSpace.Class
import Formulative.Postprocess.Export.Types

class (VectorSpace a) => HasObjectiveFunctionM m a where
    getObjectiveFunctionM :: m (a -> Scalar a)
    default getObjectiveFunctionM :: (Has (Variable a) sig m, InnerProductSpace a, HasGradObjectiveFunctionM m a) => m (a -> Scalar a)
    getObjectiveFunctionM = do
        xOld <- getVariableOld @a
        gradf <- getGradientOfObjectiveFunctionM
        return $ \x -> (x .-. xOld) <.> gradf x

class (VectorSpace a) => HasGradObjectiveFunctionM m a where
    getGradientOfObjectiveFunctionM :: m (a -> a)

-- initial condition
class HasInitialConditionM m a where
    getInitialConditionM :: m a
class HasEqualityConstraintM m a where
    type EqualityConstraintType a :: *
    getEqualityConstraintM :: m (a -> EqualityConstraintType a)
class HasGradPenaltyM m a where
    getGradPenaltyM :: m (LagrangianMultiplier (EqualityConstraintType a) -> a -> a)

class (HasInitialConditionM m a) => HasUpdateM m a where
    updateM :: a -> m a

class HasExportStaticsM m a where
    exportStaticsM :: a -> m ()

-- class HasExportDynamicsM m a where
--     exportDynamicsM :: IndexOfStep -> DynamicParameter b -> a -> m ()
--     default exportDynamicsM :: IndexOfStep -> DynamicParameter b -> a -> m ()
--     exportDynamicsM (IndexOfStep i) (DynamicParameter t) xi = do
--         exportDynamicParameter (IndexOfStep i) (DynamicParameter t)
--         exportVariableDynamic (IndexOfStep i) xi
--         exportDependentVariableLocalDynamic (IndexOfStep i) xi
--         exportDependentVariableGlobal xi

class HasGlobalDependentVariableM m a where
    type GlobalDependentVariable a :: *
    type GlobalDependentVariable a = () -- default
    dependentVariableGlobalM :: a -> m (GlobalDependentVariable a)
    default dependentVariableGlobalM :: (Monad m, GlobalDependentVariable a ~ ()) => a -> m (GlobalDependentVariable a)
    dependentVariableGlobalM x = return ()
class HasLocalDependentVariableM m a where
    type LocalDependentVariable a :: *
    type LocalDependentVariable a = () -- default
    dependentVariableLocalM :: a -> m (LocalDependentVariable a)
    default dependentVariableLocalM :: (Monad m, LocalDependentVariable a ~ ()) => a -> m (LocalDependentVariable a)
    dependentVariableLocalM x = return ()

class HasDependentParameterM m a where
    type DependentParameterType a :: *
    type DependentParameterType a = () -- default
    dependentParameterM :: m (DependentParameterType a)
    default dependentParameterM :: (Monad m, DependentParameterType a ~ ()) => m (DependentParameterType a)
    dependentParameterM = return ()

instance DefaultOrdered ()
instance ToRecord ()
instance ToNamedRecord ()