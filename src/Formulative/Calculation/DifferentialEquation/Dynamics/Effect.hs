{-# LANGUAGE AllowAmbiguousTypes #-}

module Formulative.Calculation.DifferentialEquation.Dynamics.Effect where

import Control.Algebra
import Formulative.Calculation.DifferentialEquation.Types

data Dynamics a m k where
    AskDynamicsSetting :: Dynamics a m (DynamicsSetting a)
    AskStepSize :: Dynamics a m (StepSize a)

askDynamicsSetting :: forall a sig m. (Has (Dynamics a) sig m) => m (DynamicsSetting a)
askDynamicsSetting = send AskDynamicsSetting
askLabelOfDynamicParameter :: forall a sig m. (Has (Dynamics a) sig m) => m LabelOfDynamicParameter
askLabelOfDynamicParameter = label <$> (askDynamicsSetting @a)
askStepSize :: (Has (Dynamics a) sig m) => m (StepSize a)
askStepSize = send AskStepSize