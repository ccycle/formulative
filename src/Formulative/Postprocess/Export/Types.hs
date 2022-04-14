{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Formulative.Postprocess.Export.Types where

import Data.Hashable
import Data.String.Conversions (ConvertibleStrings (convertString))
import qualified Data.Text as T
import Dhall
import Formulative.Preprocess.DefaultValue
import GHC.Generics
import Path

-- TODO: add VTU
data ExportFormat = CSV
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall)
instance HasDefaultValue ExportFormat where
    defaultValue = CSV
instance Hashable ExportFormat where
    hashWithSalt s a = hashWithSalt s (1 :: Int)

-- Ordinary Differential Equation | Partial Differential Equation
data EquationType = ODE | PDE
    deriving stock (Generic, Show, Eq)

-- dhallから読み取った生のfilepath
newtype OutputDirSetting = OutputDirSetting FilePath
    deriving stock (Generic, Show, Eq)
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

data ExportSetting = ExportSetting {format :: ExportQuantityFormat, output :: OutputDirSetting}
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable, HasDefaultValue)

newtype IndexOfStep = IndexOfStep Natural
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable)
newtype Parameter a = Parameter a
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable)

-- configで設定できるようにする
-- 相対パスの指定かautoか
-- 過去の計算結果をもとに再計算させるなどの使い方を想定
newtype OutputDir = OutputDir (Path Rel Dir)
    deriving stock (Generic, Show, Eq)