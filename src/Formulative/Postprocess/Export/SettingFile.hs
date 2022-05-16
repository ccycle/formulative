{-# LANGUAGE AllowAmbiguousTypes #-}

module Formulative.Postprocess.Export.SettingFile where

import Control.Algebra
import Control.Effect.Lift
import Control.Effect.Sum
import Control.Monad
import qualified Data.Text.IO as T
import Dhall
import Formulative.Calculation.Internal.Class
import Formulative.Postprocess.Export.Effect
import Formulative.Postprocess.Export.IO
import Formulative.Postprocess.Export.Types
import Formulative.Preprocess.ReadSetting
import Formulative.Preprocess.SettingFile.Effect
import Path
import Path.IO

exportSettingFile ::
    forall m sig.
    ( Algebra sig m
    , Member Export sig
    , Member SettingFile sig
    , Member (Lift IO) sig
    ) =>
    m ()
exportSettingFile = do
    (OutputDir outputDir) <- askOutputDir
    (DhallSettingText x) <- askSettingFileText
    sendIO $ ensureDir outputDir
    sName <- sendIO $ parseRelFile "setting.dhall"
    let filePath = toFilePath (outputDir </> sName)
    msgLoggerM $ concat ["Exporting setting file (", filePath, ") .."]
    sendIO $ T.writeFile filePath x

exportDependentParameterFile ::
    forall a m sig.
    ( Algebra sig m
    , Member Export sig
    , Member (Lift IO) sig
    , HasDependentParameterM m a
    , ToDhall (DependentParameterType a)
    ) =>
    m ()
exportDependentParameterFile = do
    x <- dependentParameterM @m @a
    let y = toDhallText x
    when (y /= toDhallText ()) $ do
        (OutputDir outputDir) <- askOutputDir
        sendIO $ ensureDir outputDir
        sName <- sendIO $ parseRelFile "dependentParamater.dhall"
        let filePath = toFilePath (outputDir </> sName)
        msgLoggerM $ concat ["Exporting dependent parameter file (", filePath, ") .."]
        sendIO $ T.writeFile filePath y