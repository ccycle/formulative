{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Formulative.Postprocess.Export.Types where

import Data.Csv (FromField)
import Data.Hashable
import Data.String.Conversions (ConvertibleStrings (convertString))
import qualified Data.Text as T
import Dhall
import Formulative.Calculation.Algebra.Arithmetic
import Formulative.Calculation.Algebra.Arithmetic.Additive
import Formulative.Calculation.Algebra.Arithmetic.Semiring
import Formulative.Preprocess.DefaultValue
import GHC.Exts (IsString)
import Path

newtype LogFileDirSetting = LogFileDirSetting String
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall)
instance HasDefaultValue LogFileDirSetting where
    defaultValue = LogFileDirSetting ("./output/" <> convertString outputDirHashCmdStr <> "/result.log")

-- dhallから読み取った生のfilepath
newtype OutputDirSetting = OutputDirSetting FilePath
    deriving stock (Generic, Show, Eq)
    deriving newtype (IsString)
    deriving anyclass (FromDhall, ToDhall)

outputDirHashCmdStr :: FilePath
outputDirHashCmdStr = "[[hash]]"
instance HasDefaultValue OutputDirSetting where
    defaultValue = OutputDirSetting ("./output/" <> convertString outputDirHashCmdStr)

data ExportSetting = ExportSetting {logFile :: LogFileDirSetting, outputDirectory :: OutputDirSetting}
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, HasDefaultValue)
instance Hashable ExportSetting where
    hashWithSalt s a = hashWithSalt s (1 :: Int)
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

newtype LogFilePath = LogFilePath (Path Rel File)
    deriving stock (Generic, Show, Eq)