{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Formulative.Postprocess.Export.Types where

import Data.Csv (FromField)
import Data.Hashable
import Data.String.Conversions (ConvertibleStrings (convertString))
import qualified Data.Text as T
import Dhall
import Formulative.Calculation.Algebra.Arithmetic.Additive
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Algebra.Arithmetic.Semiring
import Formulative.Preprocess.DefaultValue
import GHC.Exts (IsString)
import Path

-- TODO: add VTU
data ExportFormat = CSV
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall)
instance HasDefaultValue ExportFormat where
    defaultValue = CSV
instance Hashable ExportFormat where
    hashWithSalt s a = hashWithSalt s (1 :: Int)

-- dhallから読み取った生のfilepath
newtype OutputDirSetting = OutputDirSetting FilePath
    deriving stock (Generic, Show, Eq)
    deriving newtype (IsString)
    deriving anyclass (FromDhall, ToDhall)
instance Hashable OutputDirSetting where
    hashWithSalt s a = hashWithSalt s (1 :: Int)

outputDirHashCmdStr :: FilePath
outputDirHashCmdStr = "[[hash]]"
instance HasDefaultValue OutputDirSetting where
    defaultValue = OutputDirSetting ("./output/" ++ convertString outputDirHashCmdStr)

data ExportQuantityFormat = ExportQuantityFormat {variable :: ExportFormat, local :: ExportFormat, global :: ExportFormat}
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, HasDefaultValue, Hashable)

data ExportSetting = ExportSetting {format :: ExportQuantityFormat, outputDirectory :: OutputDirSetting}
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable, HasDefaultValue)

newtype StepIndex = StepIndex Natural
    deriving stock (Generic, Eq, Ord)
    deriving newtype (Show, Enum, Num, Real, Integral)
    deriving anyclass (FromDhall, ToDhall, Hashable)

newtype MaxStepIndex = MaxStepIndex StepIndex
    deriving stock (Generic, Eq, Ord)
    deriving newtype (Show, Enum, Num, Real, Integral)
    deriving anyclass (FromDhall, ToDhall, Hashable)
newtype IntervalStepIndex = IntervalStepIndex StepIndex
    deriving stock (Generic, Eq, Ord)
    deriving newtype (Show, Enum, Num, Real, Integral)
    deriving anyclass (FromDhall, ToDhall, Hashable)

newtype DynamicParameter a = DynamicParameter a
    deriving stock (Generic, Eq)
    deriving newtype (Show, Num, Enum, Ord, FromField, Fractional, Additive, Multiplicative, Semiring)
    deriving anyclass (FromDhall, ToDhall, Hashable)
newtype OutputDir = OutputDir (Path Rel Dir)
    deriving stock (Generic, Show, Eq)