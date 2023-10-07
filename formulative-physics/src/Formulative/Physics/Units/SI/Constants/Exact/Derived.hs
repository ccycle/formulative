module Formulative.Physics.Units.SI.Constants.Exact.Derived where

import Formulative.Algebra.Prelude hiding ((>*<), (>/<))
import Formulative.Physics.Units.Arithmetic
import Formulative.Physics.Units.SI.Constants.Exact.Base
import Formulative.Physics.Units.SI.System

secondRadiationConstant :: (Field a) => (Metre >*< Kelvin) a
secondRadiationConstant = planckConstant >*< lightspeed >/< boltzmannConstant

molarPlanckConstant :: Field a => (Joule >*< Second >/< Mole) a
molarPlanckConstant = planckConstant >*< avogadroConstant

firstRadiationConstant :: (Transcendental a) => (Watt >*< Square Metre) a
firstRadiationConstant = (2 * pi) *< planckConstant >*< square lightspeed

firstRadiationConstantForSpectralRadiance :: (Transcendental a) => (Watt >*< Square Metre) a
firstRadiationConstantForSpectralRadiance = 2 *< planckConstant >*< square lightspeed

efimovFactor :: (Field a) => Dimensionless a
efimovFactor = 22.7 *< dimensionless

conductanceQuantum :: (Field a) => Siemens a
conductanceQuantum = 2 *< square elementaryCharge >/< planckConstant

inverseConductanceQuantum :: (Field a) => Ohm a
inverseConductanceQuantum = dimensionless >/< conductanceQuantum

reducedPlanckConstant :: (Transcendental a) => (Joule >*< Second) a
reducedPlanckConstant = 1 / (2 * pi) *< planckConstant

josephsonConstant :: (Field a) => (Dimensionless >/< Volt >/< Second) a
josephsonConstant = 2 *< elementaryCharge >/< planckConstant

magneticFluxQuantum :: (Field a) => Weber a
magneticFluxQuantum = dimensionless >/< josephsonConstant

vonKlitzingConstant :: Field a => Ohm a
vonKlitzingConstant = planckConstant >/< square elementaryCharge

faradayConstant :: (Field a) => (Coulomb >/< Mole) a
faradayConstant = elementaryCharge >*< avogadroConstant

gasConstant :: (Field a) => (Joule >/< Mole >/< Kelvin) a
gasConstant = boltzmannConstant >*< avogadroConstant

stefanBoltzmannConstant :: (Transcendental a) => (Watt >/< Square Metre >/< Kelvin ^+ 4) a
stefanBoltzmannConstant = (2 / 15 * pi ^+ 5) *< tesseract boltzmannConstant >/< cube planckConstant >/< square lightspeed

electronVolt :: Field a => Joule a
electronVolt = elementaryCharge >/< coulomb >*< joule

diracConstant :: Transcendental a => (Kilogram >*< Square Metre >/< Second) a
diracConstant = planckConstant >/ (2 * pi)
