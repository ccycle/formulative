{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}

module Formulative.Calculation.Internal.Setting where

import Control.Carrier.Reader
import Control.Effect.Lift
import Control.Effect.Sum
import Control.Effect.Throw
import Control.Exception.Safe
import Data.Hashable
import Dhall
import Formulative.Calculation.DifferentialEquation.Types
import Formulative.Calculation.Optimization.AugmentedLagrangian
import Formulative.Calculation.Optimization.Parameter
import Formulative.Postprocess.Export.Types
import Formulative.Preprocess.DefaultValue
import Formulative.Preprocess.DiscreteExteriorCalculus.Read
import Formulative.Preprocess.ReadSetting

runEquationConstantsIO ::
    forall r m b sig.
    ( Algebra sig m
    , Member (Lift IO) sig
    , Member (Throw SomeException) sig
    , FromDhall r
    ) =>
    ReaderC r m b ->
    m b
runEquationConstantsIO f = do
    (DhallSettingText txt) <- cmdOptionToDhallSettingText
    x <- sendIO $ readRecordFromDhallFile "equation" txt
    runReader x f

data FormulativeSetting a = FormulativeSetting
    { optimization :: OptimizationParameters a
    , export :: ExportSetting
    , geometry :: GeometrySetting
    , dynamics :: DynamicParameterSetting a
    , constrainedSystem :: ConstrainedSystemParameter a
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable, HasDefaultValue)