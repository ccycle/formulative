module Formulative.Physics.Units.SI.Constants.Exact.Base where

import Formulative.Algebra.Prelude
import Formulative.Physics.Units.Arithmetic
import Formulative.Physics.Units.SI.System

hyperfineSplittingFrequencyOfCaesium133 :: FromInteger a => (Dimensionless >/< Second) a
hyperfineSplittingFrequencyOfCaesium133 = SI 9192631770

lightspeed :: FromInteger a => (Metre >/< Second) a
lightspeed = SI 299792458

planckConstant :: Field a => (Kilogram >*< Square Metre >/< Second) a
planckConstant = SI 6.62607015e-34

elementaryCharge :: Field a => (Ampere >*< Second) a
elementaryCharge = SI 1.602176634e-19

boltzmannConstant :: Field a => (Kilogram >*< Square Metre >/< Square Second >/< Kelvin) a
boltzmannConstant = SI 1.380649e-23

avogadroConstant :: (FromInteger a, Multiplicative a) => (Dimensionless >/< Mole) a
avogadroConstant = SI (602214076 * 10 ^+ 15)

luminousEfficacy :: FromInteger a => (Candela >*< (Second ^+ 3) >/< Kilogram >/< Square Metre) a
luminousEfficacy = SI 683