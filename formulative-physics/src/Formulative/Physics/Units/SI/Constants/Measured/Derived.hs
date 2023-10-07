module Formulative.Physics.Units.SI.Constants.Measured.Derived where

import Formulative.Algebra.Prelude hiding ((>*<), (>/<))
import Formulative.Physics.Units.Arithmetic
import Formulative.Physics.Units.SI.Constants.Exact
import Formulative.Physics.Units.SI.Constants.Measured.Base
import Formulative.Physics.Units.SI.System

planckLength :: (Transcendental a) => Metre a
planckLength =
    squareRoot $
        reducedPlanckConstant >/< cube lightspeed
            >*< gravitationalConstant

planckMass :: Transcendental a => Kilogram a
planckMass =
    squareRoot $
        reducedPlanckConstant
            >*< lightspeed
                >/< gravitationalConstant

planckTime :: Transcendental a => Second a
planckTime =
    squareRoot $
        reducedPlanckConstant >/< penteract lightspeed
            >*< gravitationalConstant

planckCharge :: Transcendental a => Coulomb a
planckCharge =
    elementaryCharge
        >/< squareRoot fineStructureConstant

planckTemperature :: Transcendental a => Kelvin a
planckTemperature =
    squareRoot $
        reducedPlanckConstant
            >*< penteract lightspeed
                >/< square boltzmannConstant
                >/< gravitationalConstant

magneticConstant :: Field a => (Newton >/< Square Ampere) a
magneticConstant =
    2
        *< planckConstant
        >/< lightspeed
        >/< square elementaryCharge
        >*< fineStructureConstant

electricConstant :: Field a => (Farad >/< Metre) a
electricConstant =
    (1 / 2)
        *< square elementaryCharge
        >/< planckConstant
        >/< lightspeed
        >/< fineStructureConstant

impedanceOfVacuum :: Field a => Ohm a
impedanceOfVacuum =
    2
        *< planckConstant
        >/< square elementaryCharge
        >*< fineStructureConstant

coulombConstant :: Transcendental a => (Kilogram >*< Metre ^+ 3 >/< Second ^+ 4 >/< Square Ampere) a
coulombConstant =
    lightspeed
        >*< reducedPlanckConstant
            >/< square elementaryCharge
        >*< fineStructureConstant

bohrMagneton :: Transcendental a => (Joule >/< Tesla) a
bohrMagneton =
    (1 / (8 * pi))
        *< elementaryCharge
        >*< lightspeed
        >/< rydbergConstant
        >*< square fineStructureConstant

electronMass :: Field a => Kilogram a
electronMass =
    2
        *< planckConstant
        >/< lightspeed
        >*< rydbergConstant
        >/< square fineStructureConstant

electronMolarMass :: Field a => (Kilogram >/< Mole) a
electronMolarMass =
    2
        *< planckConstant
        >*< avogadroConstant
        >/< lightspeed
        >*< rydbergConstant
        >/< square fineStructureConstant

unifiedAtomicMassUnit :: Field a => Kilogram a
unifiedAtomicMassUnit =
    2
        *< planckConstant
        >/< lightspeed
        >*< rydbergConstant
        >/< square fineStructureConstant
        >/< electronRelativeMass

molarMassConstant :: Field a => (Kilogram >/< Mole) a
molarMassConstant =
    2
        *< planckConstant
        >*< avogadroConstant
        >/< lightspeed
        >*< rydbergConstant
        >/< square fineStructureConstant
        >/< electronRelativeMass

atomicMassOfCarbon12 :: Field a => Kilogram a
atomicMassOfCarbon12 =
    24
        *< planckConstant
        >/< lightspeed
        >*< rydbergConstant
        >/< square fineStructureConstant
        >/< electronRelativeMass

molarMassOfCarbon12 :: Field a => (Kilogram >/< Mole) a
molarMassOfCarbon12 =
    24
        *< planckConstant
        >*< avogadroConstant
        >/< lightspeed
        >*< rydbergConstant
        >/< square fineStructureConstant
        >/< electronRelativeMass

nuclearMagneton :: Transcendental a => (Joule >/< Tesla) a
nuclearMagneton =
    (1 / 2)
        *< elementaryCharge
        >*< reducedPlanckConstant
        >/< protonMass

bohrRadius :: Transcendental a => Metre a
bohrRadius =
    (1 / (4 * pi))
        *< fineStructureConstant
        >/< rydbergConstant

classicalElectronRadius :: Transcendental a => Metre a
classicalElectronRadius =
    (1 / (4 * pi))
        *< cube fineStructureConstant
        >/< rydbergConstant

hartreeEnergy :: Field a => Joule a
hartreeEnergy =
    2
        *< planckConstant
        >*< lightspeed
        >*< rydbergConstant

quantumOfCirculation :: Field a => (Square Metre >/< Second) a
quantumOfCirculation =
    (1 / 4)
        *< lightspeed
        >*< square fineStructureConstant
        >/< rydbergConstant

thomsonCrossSection :: Transcendental a => Square Metre a
thomsonCrossSection =
    (1 / (6 * pi))
        *< square (cube fineStructureConstant)
        >/< square rydbergConstant