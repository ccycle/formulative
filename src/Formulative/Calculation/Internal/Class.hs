{-# LANGUAGE AllowAmbiguousTypes #-}

module Formulative.Calculation.Internal.Class where

import Data.Csv (DefaultOrdered, ToNamedRecord, ToRecord)

class HasInitialConditionM m a where
    getInitialConditionM :: m a

class (HasInitialConditionM m a) => HasUpdateM m a where
    updateM :: a -> m a

class HasExportStaticsM m a where
    exportStaticsM :: a -> m ()

class HasGlobalDependentVariableM m a where
    type GlobalDependentVariable a :: *
    type GlobalDependentVariable a = () -- default
    globalDependentVariableM :: a -> m (GlobalDependentVariable a)
    default globalDependentVariableM :: (Monad m, GlobalDependentVariable a ~ ()) => a -> m (GlobalDependentVariable a)
    globalDependentVariableM x = return ()
class HasLocalDependentVariableM m a where
    type LocalDependentVariable a :: *
    type LocalDependentVariable a = () -- default
    localDependentVariableM :: a -> m (LocalDependentVariable a)
    default localDependentVariableM :: (Monad m, LocalDependentVariable a ~ ()) => a -> m (LocalDependentVariable a)
    localDependentVariableM x = return ()

class HasDependentParameterM m a where
    type DependentParameterType a :: *
    type DependentParameterType a = () -- default
    dependentParameterM :: m (DependentParameterType a)
    default dependentParameterM :: (Monad m, DependentParameterType a ~ ()) => m (DependentParameterType a)
    dependentParameterM = return ()

instance DefaultOrdered ()
instance ToRecord ()
instance ToNamedRecord ()