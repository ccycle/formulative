module Main (main) where

import Control.Carrier.Lift
import Control.Exception.Safe
import Dhall
import Formulative.Calculation.Internal.Setting
import Formulative.Postprocess.Export.IO
import Formulative.Preprocess.DefaultValue
import Formulative.Preprocess.Exception
import Formulative.Preprocess.ReadSetting (writeDhallFile)
import Path (parseRelFile)

mainPreprocess = do
    a <- sendIO $ parseRelFile "./default_values.dhall"
    msgExportFileM a
    sendIO $ writeDhallFile "./default_values.dhall" (defaultValue @(FormulativeSetting Double))

main = runM mainPreprocess
