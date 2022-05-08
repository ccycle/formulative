{-# LANGUAGE AllowAmbiguousTypes #-}

module Formulative.Calculation.DifferentialEquation.Dynamics.Effect where

import Control.Algebra
import Control.Carrier.Throw.Either
import Control.Effect.Sum
import Control.Exception.Safe
import Formulative.Calculation.DifferentialEquation.Types
import Formulative.Postprocess.Export.Effect
import Formulative.Postprocess.Export.Types
import Path

data Dynamics a m k where
    AskDynamicsSetting :: Dynamics a m (DynamicsSetting a)
    AskStepSize :: Dynamics a m (StepSize a)

askDynamicsSetting :: forall a sig m. (Has (Dynamics a) sig m) => m (DynamicsSetting a)
askDynamicsSetting = send AskDynamicsSetting
askLabelOfDynamicParameter :: forall a sig m. (Has (Dynamics a) sig m) => m LabelOfDynamicParameter
askLabelOfDynamicParameter = label <$> (askDynamicsSetting @a)
askStepSize :: (Has (Dynamics a) sig m) => m (StepSize a)
askStepSize = send AskStepSize

askDynamicParameterPath ::
    forall a sig m.
    ( Algebra sig m
    , Member (Dynamics a) sig
    , Member (Throw SomeException) sig
    , Member Export sig
    ) =>
    m (Path Rel File)
askDynamicParameterPath = do
    s <- askDynamicsSetting @a
    p <- liftEither $ parseRelFile (unLabelOfDynamicParameter $ label s)
    p1 <- liftEither $ addExtension ".csv" p
    (OutputDir dir) <- askOutputDir
    return (dir </> p1)