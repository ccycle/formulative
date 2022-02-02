{-# LANGUAGE DeriveAnyClass #-}

module Test.ToRecord where

import Data.Csv
import GHC.Generics
import HStructure.Postprocess.Export.Class

data RecTest1 a b = MkRecTest1 a b
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromRecord, ToRecord, ToNamedRecord)
data RecTest2 a b = MkRecTest2 {getA :: a, getB :: b}
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromRecord, ToRecord, ToNamedRecord)

testRec1 = MkRecTest1 1 1.0 :: RecTest1 Integer Double
testRec2 = MkRecTest2 1 1.0 :: RecTest2 Integer Double
toRecordTest1 = toRecord testRec1
toRecordTest2 = toRecord testRec2
toNamedRecordTest1 = toNamedRecord testRec1 -- not worked
toNamedRecordTest2 = toNamedRecord testRec2
nameListTest2 = namesOfRecord testRec2