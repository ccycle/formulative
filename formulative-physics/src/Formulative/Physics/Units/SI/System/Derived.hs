module Formulative.Physics.Units.SI.System.Derived where

import Formulative.Algebra.Prelude
import Formulative.Physics.Units.Arithmetic
import Formulative.Physics.Units.Prefixes (milli)
import Formulative.Physics.Units.SI.System.Base (Kilogram, SI (..), kilogram)

gram :: (Algebraic a) => Kilogram a
gram = milli kilogram

type Siemens = SI N2 N1 P3 P2 Z Z Z
siemens :: Multiplicative a => Siemens a
siemens = SI one

type Farad = SI N2 N1 P4 P2 Z Z Z
farad :: Multiplicative a => Farad a
farad = SI one

type Lux = SI N2 Z Z Z Z Z P1
lux :: Multiplicative a => Lux a
lux = SI one

type Pascal = SI N1 P1 N2 Z Z Z Z
pascal :: Multiplicative a => Pascal a
pascal = SI one

type Hertz = SI Z Z N1 Z Z Z Z
hertz :: Multiplicative a => Hertz a
hertz = SI one

type Becquerel = SI Z Z N1 Z Z Z Z
becquerel :: Multiplicative a => Becquerel a
becquerel = SI one

type Katal = SI Z Z N1 Z Z P1 Z
katal :: Multiplicative a => Katal a
katal = SI one

type Radian = SI Z Z Z Z Z Z Z
radian :: Multiplicative a => Radian a
radian = SI one

type Steradian = SI Z Z Z Z Z Z Z
steradian :: Multiplicative a => Steradian a
steradian = SI one

type Lumen = SI Z Z Z Z Z Z P1
lumen :: Multiplicative a => Lumen a
lumen = SI one

type Coulomb = SI Z Z P1 P1 Z Z Z
coulomb :: Multiplicative a => Coulomb a
coulomb = SI one

type Tesla = SI Z P1 N2 N1 Z Z Z
tesla :: Multiplicative a => Tesla a
tesla = SI one

type Newton = SI P1 P1 N2 Z Z Z Z
newton :: Multiplicative a => Newton a
newton = SI one

type Gray = SI P2 Z N2 Z Z Z Z
gray :: Multiplicative a => Gray a
gray = SI one

type Sievert = SI P2 Z N2 Z Z Z Z
sievert :: Multiplicative a => Sievert a
sievert = SI one

type Ohm = SI P2 P1 N3 N2 Z Z Z
ohm :: Multiplicative a => Ohm a
ohm = SI one

type Volt = SI P2 P1 N3 N1 Z Z Z
volt :: Multiplicative a => Volt a
volt = SI one

type Watt = SI P2 P1 N3 Z Z Z Z
watt :: Multiplicative a => Watt a
watt = SI one

type Henry = SI P2 P1 N2 N2 Z Z Z
henry :: Multiplicative a => Henry a
henry = SI one

type Weber = SI P2 P1 N2 N1 Z Z Z
weber :: Multiplicative a => Weber a
weber = SI one

type Joule = SI P2 P1 N2 Z Z Z Z
joule :: Multiplicative a => Joule a
joule = SI one

type instance Pretty (SI N2 N1 P3 P2 Z Z Z) = Siemens
type instance Pretty (SI N2 N1 P4 P2 Z Z Z) = Farad
type instance Pretty (SI N2 Z Z Z Z Z P1) = Lux
type instance Pretty (SI N1 P1 N2 Z Z Z Z) = Pascal
type instance Pretty (SI Z Z N1 Z Z P1 Z) = Katal
type instance Pretty (SI Z Z P1 P1 Z Z Z) = Coulomb
type instance Pretty (SI Z P1 N2 N1 Z Z Z) = Tesla
type instance Pretty (SI Z P1 Z Z Z Z Z) = Kilogram
type instance Pretty (SI P1 P1 N2 Z Z Z Z) = Newton
type instance Pretty (SI P2 P1 N3 N2 Z Z Z) = Ohm
type instance Pretty (SI P2 P1 N3 N1 Z Z Z) = Volt
type instance Pretty (SI P2 P1 N3 Z Z Z Z) = Watt
type instance Pretty (SI P2 P1 N2 N2 Z Z Z) = Henry
type instance Pretty (SI P2 P1 N2 N1 Z Z Z) = Weber
type instance Pretty (SI P2 P1 N2 Z Z Z Z) = Joule