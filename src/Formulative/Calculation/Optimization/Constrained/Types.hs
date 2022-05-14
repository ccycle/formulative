{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.Optimization.Constrained.Types where

import Control.Applicative
import Data.Coerce
import Data.Csv
import Data.Hashable
import Data.Proxy
import Dhall
import Formulative.Calculation.Internal.Class
import Formulative.Postprocess.Export.Variable.Class
import Formulative.Postprocess.Export.Variable.Local
import Formulative.Preprocess.DefaultValue
import Formulative.Preprocess.ReadFile
import GHC.Generics

data VariablesConstrainedSystem a b = VariablesConstrainedSystem {variable :: a, multiplier :: b}
    deriving stock (Show, Generic)
instance (ToVariableTypes a, ToVariableTypes b) => ToVariableTypes (VariablesConstrainedSystem a b) where
    toVariableTypes _ = toVariableTypes (Proxy @a) <> toVariableTypes (Proxy @b)
instance (DefaultOrdered a, DefaultOrdered b) => DefaultOrdered (VariablesConstrainedSystem a b) where
    headerOrder (VariablesConstrainedSystem x y) = headerOrder x <> headerOrder y
instance (ToLazyFields a, ToLazyFields b) => ToLazyFields (VariablesConstrainedSystem a b) where
    toLazyFields (VariablesConstrainedSystem x y) = toLazyFields x <> toLazyFields y
instance (FromLazyFields a, FromLazyFields b) => FromLazyFields (VariablesConstrainedSystem a b) where
    parseLazyFields v = liftA2 VariablesConstrainedSystem (parseLazyFields v) (parseLazyFields v)
    getLazyRecordsDynamics i j v = liftA2 (liftA2 (liftA2 VariablesConstrainedSystem)) (getLazyRecordsDynamics i j v) (getLazyRecordsDynamics i j v)
instance (Applicative m, HasInitialConditionM m a, HasInitialConditionM m b) => HasInitialConditionM m (VariablesConstrainedSystem a b) where
    getInitialConditionM = liftA2 VariablesConstrainedSystem getInitialConditionM getInitialConditionM

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