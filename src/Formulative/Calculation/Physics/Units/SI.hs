{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.Physics.Units.SI where

import Formulative.Calculation.Algebra.Arithmetic.Class
import Physics.Units.Arithmetic
import Physics.Units.Type

deriving newtype instance (Additive a) => Additive (SI metre kilogram second ampere kelvin mole candela a)
deriving newtype instance (AdditiveGroup a) => AdditiveGroup (SI metre kilogram second ampere kelvin mole candela a)

instance
    ( Mul a b c
    , (SI metre1 kilogram1 second1 ampere1 kelvin1 mole1 candela1 >*< SI metre2 kilogram2 second2 ampere2 kelvin2 mole2 candela2) ~ SI metre3 kilogram3 second3 ampere3 kelvin3 mole3 candela3
    , SI metre1 kilogram1 second1 ampere1 kelvin1 mole1 candela1 ~ (SI metre3 kilogram3 second3 ampere3 kelvin3 mole3 candela3 >/< SI metre2 kilogram2 second2 ampere2 kelvin2 mole2 candela2)
    , SI metre2 kilogram2 second2 ampere2 kelvin2 mole2 candela2 ~ (SI metre3 kilogram3 second3 ampere3 kelvin3 mole3 candela3 >/< SI metre1 kilogram1 second1 ampere1 kelvin1 mole1 candela1)
    ) =>
    Mul (SI metre1 kilogram1 second1 ampere1 kelvin1 mole1 candela1 a) (SI metre2 kilogram2 second2 ampere2 kelvin2 mole2 candela2 b) (SI metre3 kilogram3 second3 ampere3 kelvin3 mole3 candela3 c)
    where
    (.@.) (SI x) (SI y) = SI (x .@. y)

-- a/b=c
-- a = bc
-- b = a/c
instance
    ( Div a b c
    , (SI metre1 kilogram1 second1 ampere1 kelvin1 mole1 candela1 >/< SI metre2 kilogram2 second2 ampere2 kelvin2 mole2 candela2) ~ SI metre3 kilogram3 second3 ampere3 kelvin3 mole3 candela3
    , SI metre1 kilogram1 second1 ampere1 kelvin1 mole1 candela1 ~ (SI metre3 kilogram3 second3 ampere3 kelvin3 mole3 candela3 >*< SI metre2 kilogram2 second2 ampere2 kelvin2 mole2 candela2)
    , SI metre2 kilogram2 second2 ampere2 kelvin2 mole2 candela2 ~ (SI metre1 kilogram1 second1 ampere1 kelvin1 mole1 candela1 >*< SI metre3 kilogram3 second3 ampere3 kelvin3 mole3 candela3)
    ) =>
    Div (SI metre1 kilogram1 second1 ampere1 kelvin1 mole1 candela1 a) (SI metre2 kilogram2 second2 ampere2 kelvin2 mole2 candela2 b) (SI metre3 kilogram3 second3 ampere3 kelvin3 mole3 candela3 c)
    where
    (.//.) (SI x) (SI y) = SI (x .//. y)