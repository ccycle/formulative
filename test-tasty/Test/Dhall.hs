{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Dhall where

import Control.Exception.Safe
import Data.Csv
import Data.Hashable
import Data.Text (pack)
import Data.Void
import Dhall
import Dhall.Core
import Dhall.Pretty
import Dhall.Src
import Formulative.Calculation.Internal.TH
import Formulative.Preprocess.Exception
import Formulative.Preprocess.ReadSetting
import qualified Prettyprinter.Render.Text as Prettyprint.Text
import Test.Tasty

{- $( derivingSetting
     [d|
         data ParameterTest a = ParameterTest {parameterTest1 :: a, parameterTest2 :: a}
             deriving anyclass (ToNamedRecord, ToJSON)
         |]
  )
-}

data ParameterTest a = ParameterTest {parameterTest1 :: a, parameterTest2 :: a}
    deriving anyclass (ToNamedRecord)
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable, ToRecord)

-- data Setting = Setting {parameters :: ConstantsOfSystems, meshPath :: MeshPath}
--     deriving stock (Generic, Show, Eq)
--     deriving anyclass (FromDhall, ToDhall, Hashable)
data ConstantsOfSystems = ConstantsOfSystems {a :: ParameterTest Double, b :: Double}
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable)
defaultConstantsOfSystems = ConstantsOfSystems (ParameterTest 0.1 1e20) 0.0

-- unit_preprocess = do
--     params :: ConstantsOfSystems <- input auto "./test-tasty/Test/input/test.dhall"
--     print params
--     setting :: Setting <- input auto "./test-tasty/Test/input/mainSettingSample.dhall"
--     print setting

-- optimParam :: LineSearchParameters Double <- input auto (fillInMissingValues "./test-tasty/Test/input/missingValueTest.dhall" (defaultLineSearchParameters))
-- print optimParam

-- Configを読み取る
-- メッシュなどに異常がないか確認

-- preprocess = runReader (...) $ do
--  checkMesh
--  ...
--
-- data UnionTypeTest = CSV