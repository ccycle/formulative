{-# LANGUAGE DeriveAnyClass #-}

module Test.ReadFile where

import Control.Algebra
import Control.Carrier.Lift
import Control.Effect.Lift
import Control.Effect.Sum
import Control.Effect.Throw
import Control.Exception.Safe
import qualified Data.ByteString.Lazy as BSL
import Data.Csv
import Data.Hashable
import qualified Data.Vector as V
import Dhall
import Formulative.Internal.ReExport.Carrier
import Formulative.Postprocess.Export.Effect
import Formulative.Postprocess.Export.Types
import Formulative.Preprocess.DefaultValue
import Formulative.Preprocess.Exception
import Formulative.Preprocess.ReadFile

data TestData = TestData {v1 :: Double, v2 :: Double}
    deriving stock (Show, Generic)
    deriving anyclass (DefaultOrdered, FromLazyFields)

vecTest1 :: V.Vector BSL.ByteString
vecTest1 = V.fromList ["0.1", "1.0"]
vecTest2 :: V.Vector BSL.ByteString
vecTest2 = V.fromList ["0.1", "1 0"]

-- parseTest1 = parseLazyFields @TestData vecTest1
-- parseTest2 = parseLazyFields @TestData vecTest2

getLazyRecordsTest :: (Algebra sig m, Member (Throw SomeException) sig, Member (Lift IO) sig, Member Export sig) => m [Either ReadRecordException TestData]
getLazyRecordsTest = getLazyRecordsDynamicsM 1 2

getLazyRecordsTestIO :: IO ()
getLazyRecordsTestIO = runM
    . runSomeException printSomeException
    . runSettingFile ()
    . runExport defaultValue{outputDirectory = "test-tasty/Test/ReadFile/"}
    $ do
        val <- getLazyRecordsTest
        sendIO $ print val