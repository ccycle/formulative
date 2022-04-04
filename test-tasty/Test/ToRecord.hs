{-# LANGUAGE DeriveAnyClass #-}

module Test.ToRecord where

import Control.Carrier.Error.Either
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Exception.Safe
import Data.Csv
import qualified Data.Vector as V
import Dhall
import GHC.Generics
import OptDEC.Calculation.Algebra.Arithmetic.Class
import OptDEC.Calculation.DiscreteExteriorCalculus.DifferentialForm.Proofs
import OptDEC.Calculation.DiscreteExteriorCalculus.DifferentialForm.Types
import OptDEC.Calculation.VectorSpace.Class
import OptDEC.Postprocess.Export.Class

-- import OptDEC.Preprocess.Exception

import OptDEC.Postprocess.Export.ToRecords
import Path

data RecTest1 a b = MkRecTest1 a b
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromRecord, ToRecord, FromDhall, ToDhall, ToNamedRecord)

-- data RecTest2 a b = MkRecTest2 {getA :: a, getB :: b}
--     deriving stock (Generic, Show, Eq)
--     deriving anyclass (FromRecord, ToRecord, ToNamedRecord)

data DFormRecTest = MkDFormTest {mat1 :: DifferentialForm 2 '[4, 5, 2] Primal 0 Double, mat2 :: DifferentialForm 2 '[4, 5, 2] Primal 1 Double}
    deriving stock (Show, Generic)
    deriving anyclass (Additive, AdditiveGroup, VectorSpace, ToRecord)

newtype GlobalQuantityTest a = MkGlobalQuantityTest a
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromRecord, ToRecord, ToNamedRecord)

testRec1 = MkRecTest1 1 1.0 :: RecTest1 Integer Double

-- testRec2 = MkRecTest2 1 1.0 :: RecTest2 Integer Double
toRecordTest1 = toRecord testRec1

-- toRecordTest2 = toRecord testRec2
-- toNamedRecordTest1 = toNamedRecord testRec1 -- not worked
-- toNamedRecordTest2 = toNamedRecord testRec2

-- nameListTest2 = namesOfRecord testRec2

matTest = one :: DifferentialForm 2 '[4, 5, 2] Primal 0 Double
dFormTest = MkDFormTest one one
toRecordDFormTest = toRecord dFormTest

-- toNamedRecordTest3 = toNamedRecord dFormTest

parentDirTest :: MonadThrow m => m (Path Rel Dir)
parentDirTest = parseRelDir "writeFileTestDir"

-- import Data.Csv
-- import qualified Data.Vector as V
data ToRecordListTest = ToRecordListTest {v1 :: V.Vector Double, v2 :: V.Vector Double}
    deriving stock (Generic, Show)
    deriving anyclass (ToRecords, DefaultOrdered)
a = V.fromList [0, 1, 2] :: V.Vector Double
v = ToRecordListTest a a
toRecordsTest = toRecords v
toRecordsTest1 = V.head toRecordsTest