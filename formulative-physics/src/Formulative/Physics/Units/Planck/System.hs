{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Physics.Units.Planck.System where

import Data.Aeson.Types (FromJSON, ToJSON)
import Dhall (FromDhall, ToDhall)
import Formulative.Algebra.Prelude
import Formulative.Algebra.Vector.NormSpace (NormSpace (..))
import Formulative.Algebra.Vector.VectorSpace
import Formulative.Physics.Units.Arithmetic
import Formulative.Physics.Units.Prefixes (milli)
import GHC.Generics
import GHC.TypeLits (AppendSymbol, KnownSymbol)
import qualified Physics.Units as Physics (Planck (..))

newtype Planck (metre :: Exponent) (kilogram :: Exponent) (second :: Exponent) (coulomb :: Exponent) (kelvin :: Exponent) a = Planck a
    deriving stock (Eq, Generic, Functor)
    deriving newtype (ToJSON, FromJSON, FromDhall, ToDhall)
    deriving newtype (Additive, AdditiveGroup, VectorSpace, NormSpace)

instance
    ( Show a
    , KnownSymbol z
    , z ~ AppendSymbol (ShowUnit "m\8346" metre) (AppendSymbol (ShowUnit "kg\8346" kilogram) (AppendSymbol (ShowUnit "s\8346" second) (AppendSymbol (ShowUnit "C\8346" coulomb) (ShowUnit "K\8346" kelvin))))
    ) =>
    Show (Planck metre kilogram second coulomb kelvin a)
    where
    show (Planck x) = show (Physics.Planck x :: Physics.Planck metre kilogram second coulomb kelvin a)

instance Applicative (Planck metre kilogram second coulomb kelvin) where
    pure = Planck
    Planck f <*> x = f <$> x

type Dimensionless = Planck Z Z Z Z Z
dimensionless :: (Multiplicative a) => Metre a
dimensionless = Planck one

-- base unit
type Metre = Planck P1 Z Z Z Z
metre :: (Multiplicative a) => Metre a
metre = Planck one

type Kilogram = Planck Z P1 Z Z Z
kilogram :: (Multiplicative a) => Kilogram a
kilogram = Planck one

gram :: (Algebraic a) => Kilogram a
gram = milli kilogram

type Second = Planck Z Z P1 Z Z
second :: (Multiplicative a) => Second a
second = Planck one

type Coulomb = Planck Z Z Z P1 Z
coulomb :: (Multiplicative a) => Coulomb a
coulomb = Planck one

type Kelvin = Planck Z Z Z Z P1
kelvin :: (Multiplicative a) => Kelvin a
kelvin = Planck one

-- derived unit
type Siemens = Planck N2 N1 P3 P2 Z
siemens :: Multiplicative a => Siemens a
siemens = Planck one

type Farad = Planck N2 N1 P4 P2 Z
farad :: Multiplicative a => Farad a
farad = Planck one

type Pascal = Planck N1 P1 N2 Z Z
pascal :: Multiplicative a => Pascal a
pascal = Planck one

type Hertz = Planck Z Z N1 Z Z
hertz :: Multiplicative a => Hertz a
hertz = Planck one

type Becquerel = Planck Z Z N1 Z Z
becquerel :: Multiplicative a => Becquerel a
becquerel = Planck one

type Radian = Planck Z Z Z Z Z
radian :: Multiplicative a => Radian a
radian = Planck one

type Steradian = Planck Z Z Z Z Z
steradian :: Multiplicative a => Steradian a
steradian = Planck one

type Tesla = Planck Z P1 N2 N1 Z
tesla :: Multiplicative a => Tesla a
tesla = Planck one

type Newton = Planck P1 P1 N2 Z Z
newton :: Multiplicative a => Newton a
newton = Planck one

type Gray = Planck P2 Z N2 Z Z
gray :: Multiplicative a => Gray a
gray = Planck one

type Sievert = Planck P2 Z N2 Z Z
sievert :: Multiplicative a => Sievert a
sievert = Planck one

type Ohm = Planck P2 P1 N3 N2 Z
ohm :: Multiplicative a => Ohm a
ohm = Planck one

type Volt = Planck P2 P1 N3 N1 Z
volt :: Multiplicative a => Volt a
volt = Planck one

type Watt = Planck P2 P1 N3 Z Z
watt :: Multiplicative a => Watt a
watt = Planck one

type Henry = Planck P2 P1 N2 N2 Z
henry :: Multiplicative a => Henry a
henry = Planck one

type Weber = Planck P2 P1 N2 N1 Z
weber :: Multiplicative a => Weber a
weber = Planck one

type Joule = Planck P2 P1 N2 Z Z
joule :: Multiplicative a => Joule a
joule = Planck one

type instance Pretty (Planck N2 N1 P3 P2 Z) = Siemens
type instance Pretty (Planck N2 N1 P4 P2 Z) = Farad
type instance Pretty (Planck N1 P1 N2 Z Z) = Pascal
type instance Pretty (Planck Z Z Z Z Z) = Dimensionless
type instance Pretty (Planck Z Z Z Z P1) = Kelvin
type instance Pretty (Planck Z Z Z P1 Z) = Coulomb
type instance Pretty (Planck Z Z P1 Z Z) = Second
type instance Pretty (Planck Z P1 N2 N1 Z) = Tesla
type instance Pretty (Planck Z P1 Z Z Z) = Kilogram
type instance Pretty (Planck P1 Z Z Z Z) = Metre
type instance Pretty (Planck P1 P1 N2 Z Z) = Newton
type instance Pretty (Planck P2 P1 N3 N2 Z) = Ohm
type instance Pretty (Planck P2 P1 N3 N1 Z) = Volt
type instance Pretty (Planck P2 P1 N3 Z Z) = Watt
type instance Pretty (Planck P2 P1 N2 N2 Z) = Henry
type instance Pretty (Planck P2 P1 N2 N1 Z) = Weber
type instance Pretty (Planck P2 P1 N2 Z Z) = Joule

type instance Planck i ii iii iv v >*< Planck i' ii' iii' iv' v' = Pretty (Planck (Plus i i') (Plus ii ii') (Plus iii iii') (Plus iv iv') (Plus v v'))
type instance Planck i ii iii iv v >/< Planck i' ii' iii' iv' v' = Pretty (Planck (Minus i i') (Minus ii ii') (Minus iii iii') (Minus iv iv') (Minus v v'))

type instance NthRoot n (Planck i ii iii iv v) = Planck (Divide i n) (Divide ii n) (Divide iii n) (Divide iv n) (Divide v n)

-- c = a*b
-- a = c/b
-- b = c/a
instance
    ( Multiplicative a
    , Planck metre3 kilogram3 second3 coulomb3 kelvin3 ~ (Planck metre1 kilogram1 second1 coulomb1 kelvin1 >*< Planck metre2 kilogram2 second2 coulomb2 kelvin2)
    , Planck metre1 kilogram1 second1 coulomb1 kelvin1 ~ (Planck metre3 kilogram3 second3 coulomb3 kelvin3 >/< Planck metre2 kilogram2 second2 coulomb2 kelvin2)
    , Planck metre2 kilogram2 second2 coulomb2 kelvin2 ~ (Planck metre3 kilogram3 second3 coulomb3 kelvin3 >/< Planck metre1 kilogram1 second1 coulomb1 kelvin1)
    ) =>
    Mul (Planck metre1 kilogram1 second1 coulomb1 kelvin1 a) (Planck metre2 kilogram2 second2 coulomb2 kelvin2 a) (Planck metre3 kilogram3 second3 coulomb3 kelvin3 a)
    where
    (>*<) (Planck x) (Planck y) = Planck (x * y)

-- c = a/b
-- a = b*c
-- b = a/c
instance
    ( Field a
    , Planck metre3 kilogram3 second3 coulomb3 kelvin3 ~ (Planck metre1 kilogram1 second1 coulomb1 kelvin1 >/< Planck metre2 kilogram2 second2 coulomb2 kelvin2)
    , Planck metre1 kilogram1 second1 coulomb1 kelvin1 ~ (Planck metre2 kilogram2 second2 coulomb2 kelvin2 >*< Planck metre3 kilogram3 second3 coulomb3 kelvin3)
    , Planck metre2 kilogram2 second2 coulomb2 kelvin2 ~ (Planck metre1 kilogram1 second1 coulomb1 kelvin1 >/< Planck metre3 kilogram3 second3 coulomb3 kelvin3)
    ) =>
    Div (Planck metre1 kilogram1 second1 coulomb1 kelvin1 a) (Planck metre2 kilogram2 second2 coulomb2 kelvin2 a) (Planck metre3 kilogram3 second3 coulomb3 kelvin3 a)
    where
    (>/<) (Planck x) (Planck y) = Planck (x / y)