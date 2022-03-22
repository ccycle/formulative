{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Dhall where

import Control.Exception.Safe
import Data.Aeson
import Data.Csv
import Data.Hashable
import Data.Text (pack)
import Data.Void
import Dhall
import Dhall.Core
import Dhall.Pretty
import Dhall.Src
import OptDEC.Calculation.Internal.TH
import OptDEC.Preprocess.DiscreteExteriorCalculus.Read
import OptDEC.Preprocess.Exception
import OptDEC.Preprocess.ReadSetting
import qualified Prettyprinter.Render.Text as Prettyprint.Text
import Test.Tasty

{- $( derivingSetting
     [d|
         data ParameterTest a = MkParameterTest {parameterTest1 :: a, parameterTest2 :: a}
             deriving anyclass (ToNamedRecord, ToJSON)
         |]
  )
-}

data ParameterTest a = MkParameterTest {parameterTest1 :: a, parameterTest2 :: a}
    deriving anyclass (ToNamedRecord, ToJSON)
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable, ToRecord)

-- data Setting = MkSetting {parameters :: ConstantsOfSystems, meshPath :: MeshPath}
--     deriving stock (Generic, Show, Eq)
--     deriving anyclass (FromDhall, ToDhall, Hashable)
data ConstantsOfSystems = MkConstantsOfSystems {a :: ParameterTest Double, b :: Double}
    deriving stock (Generic, Show, Eq)
    deriving anyclass (ToJSON)
    deriving anyclass (FromDhall, ToDhall, Hashable)
defaultConstantsOfSystems = MkConstantsOfSystems (MkParameterTest 0.1 1e20) 0.0

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