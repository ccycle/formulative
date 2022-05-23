{-# LANGUAGE DeriveAnyClass #-}

module Test.WriteFile where

import Control.Carrier.Lift
import Data.Csv
import qualified Data.Matrix.Static.LinearAlgebra as MSL
import Formulative.Calculation.Internal.List
import Formulative.Calculation.Internal.Setting
import Formulative.Calculation.Matrix.Types
import Formulative.Postprocess.Export.Carrier
import Formulative.Postprocess.Export.IO (ensureDirOutputM)
import Formulative.Postprocess.Export.Types
import Formulative.Postprocess.Export.Variable.Class
import Formulative.Postprocess.Export.Variable.Local
import Formulative.Preprocess.DefaultValue
import Formulative.Preprocess.Exception
import Formulative.Preprocess.SettingFile.Carrier
import GHC.Generics

matTest = fromList [0, 1, 2, 0] :: MSL.SparseMatrix 2 2 Double

data RecTest = RecTest {x1 :: Double, x2 :: Double, x3 :: MSL.SparseMatrix 2 2 Double}
    deriving stock (Generic)
    deriving anyclass (DefaultOrdered, ToLazyFields, ToVariableTypes)

main :: IO ()
main =
    runM . runSomeException printSomeException
        . runSettingFile (defaultValue :: FormulativeSetting Double)
        . runExport defaultValue
        $ ensureDirOutputM >> exportRecordToFilesStaticsM (RecTest 0.1 0.1 matTest)
            >> exportRecordToFilesStaticsM (RecTest 0.1 0.2 matTest)
            >> exportRecordToFilesDynamicsM 0 (RecTest 3 4 matTest)
            >> exportRecordToFilesDynamicsM 1 (RecTest 1 1 matTest)