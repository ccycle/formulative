{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}

module Formulative.Calculation.Internal.Setting where

import Control.Carrier.Reader
import Control.Effect.Lift
import Control.Effect.Sum
import Control.Effect.Throw
import Control.Exception.Safe
import Control.Monad
import Data.Hashable
import Dhall
import Formulative.Calculation.DifferentialEquation.Types
import Formulative.Calculation.Optimization.AugmentedLagrangian
import Formulative.Calculation.Optimization.Parameter
import Formulative.Postprocess.Export.Class (msgExportFileIO)
import Formulative.Postprocess.Export.Path
import Formulative.Postprocess.Export.Types
import Formulative.Preprocess.DefaultValue
import Formulative.Preprocess.DiscreteExteriorCalculus.Read
import Formulative.Preprocess.ReadSetting
import Path
import Path.IO

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
    { optimization :: OptimizationSetting a
    , export :: ExportSetting
    , geometry :: GeometrySetting
    , dynamics :: DynamicsSetting a
    , constrainedSystem :: ConstrainedSystemSetting a
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable, HasDefaultValue)

writeSettingFilesFromList path f list = forM_ list $ \a -> do
    let s = f a
    p <- parseRelDir path
    ensureDir p
    let str = concat ["setting_", hashHexadecimalString s, ".dhall"]
    fileName <- parseRelFile str
    let filePath = p </> fileName
    msgExportFileIO filePath
    writeDhallFile (toFilePath filePath) s
