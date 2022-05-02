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
import Formulative.Postprocess.Export.ToRecords
import Formulative.Postprocess.Export.Types
import Formulative.Postprocess.Export.Variable.Global
import Formulative.Preprocess.IO
import Formulative.Preprocess.SettingFile.Effect
import Path

exportVariableStatic x = do
    OutputDir parentDir <- askOutputDir
    ensureDirOutputM
    let x' = V.zip (headerOrder x) (toRecords x)
    forM_ x' $ \(key, str) -> do
        parseKey <- liftEither $ parseRelFile (convertString key)
        -- TODO: VTUに対応
        fileName <- liftEither $ replaceExtension ".csv" parseKey
        let filePath = parentDir </> fileName
        sendIO $ BSL.writeFile (toFilePath filePath) (encode [str])

exportDependentVariableLocalStatic x = do
    x' <- dependentVariableLocalM x
    exportVariableStatic x'

mainCalcStatics ::
    forall a sig m.
    ( Algebra sig m
    , Member (Lift IO) sig
    , Member (Throw SomeException) sig
    , Member Export sig
    , Member SettingFile sig
    , HasUpdateM m a
    , HasGlobalDependentVariableM m a
    , HasLocalDependentVariableM m a
    , ToRecords a
    , ToRecord (GlobalDependentVariable a)
    , ToRecords (LocalDependentVariable a)
    , DefaultOrdered a
    , DefaultOrdered (GlobalDependentVariable a)
    , DefaultOrdered (LocalDependentVariable a)
    , ToDhall (DependentParameterType a)
    , HasDependentParameterM m a
    ) =>
    m ()
mainCalcStatics = do
    preprocessM @m @a
    x <- getInitialConditionM @m @a
    msgStart
    putStrLnM "solve equation"
    x' <- updateM x
    putStrLnM "Exporting data .."
    exportVariableStatic x'
    exportDependentVariablesGlobal x'
    exportDependentVariableLocalStatic x'
    msgEnd