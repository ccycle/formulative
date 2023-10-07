module Formulative.Physics.Units.SI.Constants.Measured.Base where

import Formulative.Algebra.Prelude
import Formulative.Physics.Units.Arithmetic
import Formulative.Physics.Units.SI.System

-- Measured constants sorted by ascending precision

weakMixingAngle :: Field a => Dimensionless a
weakMixingAngle = SI 2.22290e-1 -- (30)

weinbergAngle :: Field a => Dimensionless a
weinbergAngle = weakMixingAngle

gravitationalConstant :: Field a => (Cube Metre >/< Kilogram >/< Square Second) a
gravitationalConstant = SI 6.67430e-11 -- (15)

-- (W(-3*exp(-3))+3)*k/h
wienWavelengthDisplacementLawConstant :: Field a => (Metre >*< Kelvin) a
wienWavelengthDisplacementLawConstant = SI 2.897771955e-3 -- (exact)

wienFrequencyDisplacementLawConstant :: Field a => (Hertz >/< Kelvin) a
wienFrequencyDisplacementLawConstant = SI 5.878925757e10 -- (exact)

protonMass :: Field a => Kilogram a
protonMass = SI 1.67262192369e-27 -- (21)

electronRelativeMass :: Field a => Dimensionless a
electronRelativeMass = SI 5.48579909070e-4 -- (16)

fineStructureConstant :: Field a => Dimensionless a
fineStructureConstant = SI 7.2973525693e-3 -- (17)

rydbergConstant :: Field a => (Dimensionless >/< Metre) a
rydbergConstant = SI 10973731.568160 -- (21)