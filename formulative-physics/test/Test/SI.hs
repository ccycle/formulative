{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeFamilies #-}

module Test.SI where

import Data.Proxy (Proxy (..))
import Dhall (FromDhall, Generic, Text, ToDhall, auto, input)
import Formulative.Algebra.Prelude
import Formulative.Algebra.Vector.VectorSpace (VectorSpace (..))
import Formulative.Physics.Units.Arithmetic hiding ((*<), (>*<), (>/<))
import Formulative.Physics.Units.SI.System

data TestData = TestData {a :: Metre Double, b :: Kilogram Double, c :: Second Double}
    deriving stock (Show, Generic)
    deriving anyclass (FromDhall, ToDhall)

dhallText :: Text
dhallText = "{a = 1.0, b = 2.0, c = 3.0}"

unit_fromDhallTest :: IO ()
unit_fromDhallTest = (input auto dhallText :: IO TestData) >>= print

unit_TestData :: IO ()
unit_TestData = print TestData{a = 1.0 *< metre, b = 2.0 *< kilogram, c = 3.0 *< second}

unit_Dimensionless :: IO ()
unit_Dimensionless = print $ metre @Double >/< metre @Double

unit_Div :: IO ()
unit_Div = print $ metre @Double >/< second @Double

unit_derivedUnit :: IO ()
unit_derivedUnit = print (kilogram @Double >*< metre @Double >/< (second @Double >*< second @Double) :: Newton Double)

unit_square :: IO ()
unit_square = print $ square (kilogram @Double)

unit_squareRoot :: IO ()
unit_squareRoot = print $ squareRoot (square (kilogram @Double))

unit_nthRoot :: IO ()
unit_nthRoot = print $ nthRoot (Proxy @2) (hypercube (Proxy @4) (kilogram @Double))
