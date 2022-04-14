{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Formulative.Calculation.Internal.Class where

import Control.Algebra
import Control.Carrier.State.Strict
import Data.Csv (DefaultOrdered, ToNamedRecord, ToRecord)
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Optimization.AugmentedLagrangian
import Formulative.Calculation.VectorSpace.Class

class (VectorSpace a) => HasObjectiveFunctionM m a where
    getObjectiveFunctionM :: m (a -> Scalar a)
    default getObjectiveFunctionM :: (Has (State a) sig m, InnerProductSpace a, HasGradObjectiveFunctionM m a) => m (a -> Scalar a)
    getObjectiveFunctionM = do
        xOld <- get
        gradf <- getGradientOfObjectiveFunctionM
        return $ \x -> (x .-. xOld) <.> gradf x

class (VectorSpace a) => HasGradObjectiveFunctionM m a where
    getGradientOfObjectiveFunctionM :: m (a -> a)

-- initial condition
class HasInitialConditionM m a where
    getInitialConditionM :: m a

runInitialConditionM :: forall a m b. (Monad m, HasInitialConditionM m a) => StateC a m b -> m b
runInitialConditionM f = do
    val <- getInitialConditionM
    evalState val f

class HasEqualityConstraintM m a where
    type EqualityConstraintType a :: *
    getEqualityConstraintM :: m (a -> EqualityConstraintType a)
class HasGradPenaltyM m a where
    getGradPenaltyM :: m (LagrangianMultiplier (EqualityConstraintType a) -> a -> a)

class (HasInitialConditionM m a) => HasUpdateM m a where
    updateM :: a -> m a

newtype Variable a = Variable a

class HasDependentVariableGlobalM m a where
    type DependentVariableGlobalType a :: *
    type DependentVariableGlobalType a = () -- default
    dependentVariableGlobalM :: a -> m (DependentVariableGlobalType a)
    default dependentVariableGlobalM :: (Monad m, DependentVariableGlobalType a ~ ()) => a -> m (DependentVariableGlobalType a)
    dependentVariableGlobalM x = return ()
class HasDependentVariableLocalM m a where
    type DependentVariableLocalType a :: *
    type DependentVariableLocalType a = () -- default
    dependentVariableLocalM :: a -> m (DependentVariableLocalType a)
    default dependentVariableLocalM :: (Monad m, DependentVariableLocalType a ~ ()) => a -> m (DependentVariableLocalType a)
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