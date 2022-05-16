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
import Data.Coerce (coerce)
import Data.Hashable (Hashable (hashWithSalt))
import Data.Scientific
import Data.Typeable
import Dhall
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Postprocess.Export.Types
import Formulative.Preprocess.DefaultValue
import GHC.Exts (IsString)
import Refined

newtype IsAdaptiveStepSize = IsAdaptiveStepSize Bool
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable)
data DynamicsSetting a = DynamicsSetting
    { label :: LabelOfDynamicParameter
    , initialValue :: DynamicParameter a
    , finalValue :: DynamicParameter a
    , stepSize :: StepSize a
    , interval :: IntervalStepIndex
    , maximumIterationNumber :: MaxStepIndex
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall)
instance (Fractional a) => HasDefaultValue (DynamicsSetting a) where
    defaultValue =
        DynamicsSetting
            { label = LabelOfDynamicParameter "time"
            , initialValue = 0
            , finalValue = 1
            , stepSize = 0.01
            , interval = 10
            , maximumIterationNumber = 1000
            }
instance (Hashable a, Fractional a) => Hashable (DynamicsSetting a) where
    hashWithSalt
        s
        DynamicsSetting{..} =
            s `hashWithSalt` initialValue `hashWithSalt` interval `hashWithSalt` stepSize

newtype LabelOfDynamicParameter = LabelOfDynamicParameter String
    deriving stock (Generic, Show, Eq)
    deriving newtype (IsString)
    deriving anyclass (FromDhall, ToDhall, Hashable)

unLabelOfDynamicParameter :: LabelOfDynamicParameter -> String
unLabelOfDynamicParameter = coerce
newtype StepSize a = StepSize a
    deriving stock (Generic, Eq)
    deriving newtype (Show, Num, Enum, Fractional)
    deriving anyclass (FromDhall, ToDhall, Hashable)

unStepSize :: StepSize a -> a
unStepSize = coerce

exponentBase10 x = base10Exponent (fromFloatDigits x) + (pred . length . show . coefficient) (fromFloatDigits x)

roundStepSize (StepSize dt) x = fromIntegral (round $ x .*. (t)) ./. (t)
  where
    n = if exponentBase10 x < 0 then negate (exponentBase10 x) else 0
    t = 10 ^ n

data StepSizeException
instance (Additive a, Eq a) => Predicate StepSizeException (StepSize a) where
    validate p (StepSize value) =
        if (== zero) value
            then Nothing
            else throwRefineOtherException (typeOf p) "*** StepSizeException: step size must be non-zero"

checkStepSize :: (Additive a, Eq a, MonadThrow m) => StepSize a -> m (Refined StepSizeException (StepSize a))
checkStepSize = refineThrow @StepSizeException

-- maxStepSizeはDynamicsSettingsからとってくる
data AdaptiveStepSizeSetting a = AdaptiveStepSizeSetting {minStepSize :: StepSize a, b :: a}