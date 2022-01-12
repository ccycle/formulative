{-# LANGUAGE TypeOperators #-}

module Test.Physics where

import Data.Coerce
import HStructure.Calculation.Algebra.Arithmetic.Class
import HStructure.Calculation.Physics.Units.SI
import Physics.Units
import Test.Tasty

-- import Physics.Units.Arithmetic
-- import Physics.Units.Type

-- second' :: a -> Second a
-- second' = coerce

mass :: Kilogram Double
mass = 95 *< kilogram

time :: Second Double
time = 9.58 *< second

distance :: Metre Double
distance = 100 *< metre

speed :: (Metre >/< Second) Double
speed = distance >/< time

momentum :: (Newton >*< Second) Double
momentum = mass >*< speed

kineticEnergy :: Joule Double
kineticEnergy = 1 / 2 *< mass >*< square speed
