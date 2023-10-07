module Test.Planck where

import Formulative.Algebra.Prelude
import Formulative.Physics.Units.Planck.System

unit_derivedUnit :: IO ()
unit_derivedUnit = print (kilogram @Double >*< metre @Double >/< (second @Double >*< second @Double) :: Newton Double)
