{-# LANGUAGE DeriveAnyClass #-}

module Test.Dhall where

import Control.Exception.Safe
import Data.Text (pack)
import Data.Void
import Dhall
import Dhall.Core
import Dhall.Pretty
import Dhall.Src
import HStructure.Preprocess.DiscreteExteriorCalculus.Read
import HStructure.Preprocess.Exception
import HStructure.Preprocess.ReadConfig
import qualified Prettyprinter.Render.Text as Prettyprint.Text
import Test.Tasty

newtype ParameterTest a = MkParameterTest a
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromDhall, ToDhall)

data Setting = MkSetting {parameters :: EquationParameters, meshDataPath :: MeshDataPath}
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromDhall, ToDhall)
data EquationParameters = MkEquationParameters {a :: ParameterTest Double, b :: Double}
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromDhall, ToDhall)

unit_preprocess = do
    params :: EquationParameters <- input auto "./test-tasty/Test/input/test.dhall"
    print params
    setting :: Setting <- input auto "./test-tasty/Test/input/mainSettingSample.dhall"
    print setting

-- optimParam :: LineSearchParameters Double <- input auto (fillInMissingValues "./test-tasty/Test/input/missingValueTest.dhall" (defaultLineSearchParameters))
-- print optimParam

-- Configを読み取る
-- メッシュなどに異常がないか確認

-- preprocess = runReader (...) $ do
--  checkMesh
--  ...
--