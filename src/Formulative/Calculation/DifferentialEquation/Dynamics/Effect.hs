{-# LANGUAGE AllowAmbiguousTypes #-}

module Formulative.Calculation.DifferentialEquation.Dynamics.Effect where

import Control.Algebra
import Formulative.Calculation.DifferentialEquation.Types

data Dynamics a m k where
    AskDynamicParameterSetting :: Dynamics a m (DynamicParameterSetting a)
    AskStepSize :: Dynamics a m (StepSize a)

askDynamicParameterSetting :: forall a sig m. (Has (Dynamics a) sig m) => m (DynamicParameterSetting a)
askDynamicParameterSetting = send AskDynamicParameterSetting
askLabelOfDynamicParameter :: forall a sig m. (Has (Dynamics a) sig m) => m LabelOfDynamicParameter
askLabelOfDynamicParameter = label <$> (askDynamicParameterSetting @a)
askStepSize :: (Has (Dynamics a) sig m) => m (StepSize a)
askStepSize = send AskStepSize