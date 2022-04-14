{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.DifferentialEquation.Types where

import Control.Algebra
import Control.Carrier.Reader
import Control.Effect.Sum (Member)
import Control.Exception.Safe
import Data.Hashable (Hashable (hashWithSalt))
import Data.Typeable
import Dhall
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Preprocess.DefaultValue
import GHC.Exts (IsString)
import Refined

newtype IsAdaptiveStepSize = IsAdaptiveStepSize Bool
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable)
data DynamicParameterSetting a = DynamicParameterSetting
    { label :: LabelOfDynamicParameter
    , initialValue :: a
    , finalValue :: a
    , stepSize :: a
    , interval :: Natural
    , maximumIterationNumber :: Natural
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall)
instance (Fractional a) => HasDefaultValue (DynamicParameterSetting a) where
    defaultValue =
        DynamicParameterSetting
            { label = LabelOfDynamicParameter "time"
            , initialValue = 0
            , finalValue = 1
            , stepSize = 0.01
            , interval = 10
            , maximumIterationNumber = 1000
            }
instance (Hashable a, Fractional a) => Hashable (DynamicParameterSetting a) where
    hashWithSalt
        s
        DynamicParameterSetting{..} =
            s `hashWithSalt` initialValue `hashWithSalt` interval `hashWithSalt` stepSize

type HasParameterConfig sig m a = (Algebra sig m, Member (Reader (DynamicParameterSetting a)) sig)

newtype LabelOfDynamicParameter = LabelOfDynamicParameter String
    deriving stock (Generic, Show, Eq)
    deriving newtype (IsString)
    deriving anyclass (FromDhall, ToDhall, Hashable)

newtype StepSize a = StepSize a
    deriving stock (Generic, Show, Eq)
    deriving newtype (Num, Enum)
    deriving anyclass (FromDhall, ToDhall, Hashable)

data StepSizeException
instance (Additive a, Eq a) => Predicate StepSizeException (StepSize a) where
    validate p (StepSize value) =
        if (== zero) value
            then Nothing
            else throwRefineOtherException (typeOf p) "*** StepSizeException: step size must be non-zero"

checkStepSize :: (Additive a, Eq a, MonadThrow m) => StepSize a -> m (Refined StepSizeException (StepSize a))
checkStepSize = refineThrow @StepSizeException