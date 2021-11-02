{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.ArithmetricClass where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Control.Lens
import LambDEC.Calculation.Operator.Arithmetic.Class

import Data.Extensible
import Data.Extensible.Dictionary (Instance1)

newtype MyType = MkMyType Double deriving (Show, Additive, AdditiveGroup)

a = MkMyType 1.0 :: MyType
b = MkMyType 1.0 :: MyType

-- https://scrapbox.io/haskell-shoen/extensible%2F%E5%88%9D%E5%BF%83%E8%80%85%E5%90%91%E3%81%91%E6%94%BB%E7%95%A5%E6%83%85%E5%A0%B1
type VariableRecord =
    Record
        '[ "name" >: String
         , "value" >: Double
         ]

type VariablesRecord =
    Record
        '[ "rho" >: Double
         , "velocity" >: Double
         ]

variablesTest1 :: VariablesRecord
variablesTest1 = #rho @= 1 <: #velocity @= 2 <: nil

variablesTest2 :: VariablesRecord
variablesTest2 = #rho @= 1 <: #velocity @= 2 <: nil

-- variablesAddTest = variablesTest1 .+. variablesTest2

-- instance Forall (KeyTargetAre KnownSymbol Num) xs => Num (Record xs) where
--     (+) = hzipWith (+)

-- data AdditiveTest = forall a. (Additive a) => AdditiveTest a

-- additiveTest :: [AdditiveTest]
-- additiveTest = [AdditiveTest (1.0::Double),AdditiveTest @Double 2.0]

-- instance Additive VariableRecord where
--     a .+. b = b & #value .~ (a ^. #value .+. b ^. #value)
--     zero = #name @= "zero" <: #value @= 0 <: nil

-- instance AdditiveGroup VariableRecord where
--     a .-. b = b & #value .~ (a ^. #value .-. b ^. #value)
--     negation a = a & #value .~ (negation (a ^. #value))

-- type VariableRecords =
--     Record
--         '[ "rho" >: VariableRecord
--          , "u" >: VariableRecord
--          , "p" >: VariableRecord
--          ]

-- instance Additive VariableRecords where
--     a .+. b = a & #rho .~ ((a ^. #rho) .+. (b ^. #rho)) & #u .~ ((a ^. #u) .+. (b ^. #u))
--     zero = undefined

-- extensibleRecordNewtypeTest :: VariableRecord' Double
-- extensibleRecordNewtypeTest = VariableRecord' (#name @= "test" <: #value @= 1 <: nil)

extensibleRecordTest1 :: VariableRecord
extensibleRecordTest1 = #name @= "pressure" <: #value @= 1 <: nil
extensibleRecordTest2 :: VariableRecord
extensibleRecordTest2 = #name @= "density" <: #value @= 2 <: nil

-- extensibleRecordTest3 = extensibleRecordTest1 .+. extensibleRecordTest2

unit_show1 = print a
unit_show2 = print (a .+. b)
unit_show3 = print (a .-. b)
