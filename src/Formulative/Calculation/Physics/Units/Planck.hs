{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.Physics.Units.Planck where

import Formulative.Calculation.Algebra.Arithmetic.Class
import Physics.Units.Planck

instance
    ( Mul a b c
    , (Planck metre1 kilogram1 second1 coulomb1 kelvin1 >*< Planck metre2 kilogram2 second2 coulomb2 kelvin2) ~ Planck metre3 kilogram3 second3 coulomb3 kelvin3
    , Planck metre1 kilogram1 second1 coulomb1 kelvin1 ~ (Planck metre3 kilogram3 second3 coulomb3 kelvin3 >/< Planck metre2 kilogram2 second2 coulomb2 kelvin2)
    , Planck metre2 kilogram2 second2 coulomb2 kelvin2 ~ (Planck metre3 kilogram3 second3 coulomb3 kelvin3 >/< Planck metre1 kilogram1 second1 coulomb1 kelvin1)
    ) =>
    Mul (Planck metre1 kilogram1 second1 coulomb1 kelvin1 a) (Planck metre2 kilogram2 second2 coulomb2 kelvin2 b) (Planck metre3 kilogram3 second3 coulomb3 kelvin3 c)
    where
    (.@.) (Planck x) (Planck y) = Planck (x .@. y)

-- a/b=c
-- a = bc
-- b = a/c

instance
    ( Div a b c
    , (Planck metre1 kilogram1 second1 coulomb1 kelvin1 >/< Planck metre2 kilogram2 second2 coulomb2 kelvin2) ~ Planck metre3 kilogram3 second3 coulomb3 kelvin3
    , Planck metre1 kilogram1 second1 coulomb1 kelvin1 ~ (Planck metre3 kilogram3 second3 coulomb3 kelvin3 >*< Planck metre2 kilogram2 second2 coulomb2 kelvin2)
    , Planck metre2 kilogram2 second2 coulomb2 kelvin2 ~ (Planck metre1 kilogram1 second1 coulomb1 kelvin1 >/< Planck metre3 kilogram3 second3 coulomb3 kelvin3)
    ) =>
    Div (Planck metre1 kilogram1 second1 coulomb1 kelvin1 a) (Planck metre2 kilogram2 second2 coulomb2 kelvin2 b) (Planck metre3 kilogram3 second3 coulomb3 kelvin3 c)
    where
    (.//.) (Planck x) (Planck y) = Planck (x .//. y)