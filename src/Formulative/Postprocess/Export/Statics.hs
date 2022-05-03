{-# LANGUAGE AllowAmbiguousTypes #-}

module Formulative.Postprocess.Export.Statics where

import Control.Effect.Error
import Control.Effect.Lift
import Control.Effect.Sum
import Control.Exception.Safe
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import Data.Csv
import Data.String.Conversions
import qualified Data.Vector as V
import Dhall
import Formulative.Calculation.Internal.Class
import Formulative.Calculation.Internal.List
import Formulative.Postprocess.Export.Effect
import Formulative.Postprocess.Export.IO
import Formulative.Postprocess.Export.Types
import Formulative.Postprocess.Export.Variable.Global
import Formulative.Postprocess.Export.Variable.Local
import Formulative.Preprocess.IO
import Formulative.Preprocess.SettingFile.Effect

exportDependentVariableLocalStatic x = do
    x' <- localDependentVariableM x
    exportRecordToFilesStaticsM x'

mainCalcStatics ::
    forall a sig m.
    ( Algebra sig m
    , Member (Lift IO) sig
    , Member (Throw SomeException) sig
    , Member Export sig
    , Member SettingFile sig
    , HasUpdateM m a
    , HasDependentParameterM m a
    , HasGlobalDependentVariableM m a
    , HasLocalDependentVariableM m a
    , ToRecord (GlobalDependentVariable a)
    , DefaultOrdered a
    , DefaultOrdered (GlobalDependentVariable a)
    , DefaultOrdered (LocalDependentVariable a)
    , ToDhall (DependentParameterType a)
    , ExportRecordToFiles a
    , ExportRecordToFiles (LocalDependentVariable a)
    ) =>
    m ()
mainCalcStatics = do
    preprocessM @m @a
    x <- getInitialConditionM @m @a
    msgStart
    putStrLnM "solve equation"
    -- TODO: updateMを使わない形にする
    -- updateMは本来Dynamicsに使う関数
    -- (SolveM classを作成する?)
    -- solveM :: m a
    x' <- updateM x
    putStrLnM "Exporting data .."
    exportRecordToFilesStaticsM x'
    exportGlobalDependentVariable x'
    exportDependentVariableLocalStatic x'
    msgEnd