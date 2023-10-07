{-# LANGUAGE UndecidableInstances #-}

module Formulative.Physics.Units.SI.System.Base where

import Data.Aeson
import Dhall
import Formulative.Algebra.Prelude
import Formulative.Algebra.Vector
import Formulative.Physics.Units.Arithmetic
import GHC.TypeLits (AppendSymbol, KnownSymbol)
import qualified Physics.Units.Type as Physics

newtype SI (metre :: Exponent) (kilogram :: Exponent) (second :: Exponent) (ampere :: Exponent) (kelvin :: Exponent) (mole :: Exponent) (candela :: Exponent) a = SI a
    deriving stock (Eq, Generic, Functor)
    deriving newtype (ToJSON, FromJSON, FromDhall, ToDhall)
    deriving newtype (Additive, AdditiveGroup, VectorSpace, NormSpace)

instance
    ( Show a
    , KnownSymbol z
    , z ~ AppendSymbol (ShowUnit "m" metre) (AppendSymbol (ShowUnit "kg" kilogram) (AppendSymbol (ShowUnit "s" second) (AppendSymbol (ShowUnit "A" ampere) (AppendSymbol (ShowUnit "K" kelvin) (AppendSymbol (ShowUnit "mol" mole) (ShowUnit "cd" candela))))))
    ) =>
    Show (SI metre kilogram second ampere kelvin mole candela a)
    where
    show (SI x) = show (Physics.SI x :: Physics.SI metre kilogram second ampere kelvin mole candela a)

instance Applicative (SI metre kilogram second ampere kelvin mole candela) where
    pure = SI
    SI f <*> x = f <$> x

type instance SI i ii iii iv v vi vii >*< SI i' ii' iii' iv' v' vi' vii' = Pretty (SI (Plus i i') (Plus ii ii') (Plus iii iii') (Plus iv iv') (Plus v v') (Plus vi vi') (Plus vii vii'))
type instance SI i ii iii iv v vi vii >/< SI i' ii' iii' iv' v' vi' vii' = Pretty (SI (Minus i i') (Minus ii ii') (Minus iii iii') (Minus iv iv') (Minus v v') (Minus vi vi') (Minus vii vii'))

type instance NthRoot n (SI i ii iii iv v vi vii) = SI (Divide i n) (Divide ii n) (Divide iii n) (Divide iv n) (Divide v n) (Divide vi n) (Divide vii n)

-- c = a*b
-- a = c/b
-- b = c/a
instance
    ( Multiplicative a
    , SI metre3 kilogram3 second3 ampere3 kelvin3 mole3 candela3 ~ (SI metre1 kilogram1 second1 ampere1 kelvin1 mole1 candela1 >*< SI metre2 kilogram2 second2 ampere2 kelvin2 mole2 candela2)
    , SI metre1 kilogram1 second1 ampere1 kelvin1 mole1 candela1 ~ (SI metre3 kilogram3 second3 ampere3 kelvin3 mole3 candela3 >/< SI metre2 kilogram2 second2 ampere2 kelvin2 mole2 candela2)
    , SI metre2 kilogram2 second2 ampere2 kelvin2 mole2 candela2 ~ (SI metre3 kilogram3 second3 ampere3 kelvin3 mole3 candela3 >/< SI metre1 kilogram1 second1 ampere1 kelvin1 mole1 candela1)
    ) =>
    Mul (SI metre1 kilogram1 second1 ampere1 kelvin1 mole1 candela1 a) (SI metre2 kilogram2 second2 ampere2 kelvin2 mole2 candela2 a) (SI metre3 kilogram3 second3 ampere3 kelvin3 mole3 candela3 a)
    where
    (>*<) (SI x) (SI y) = SI (x * y)

-- c = a/b
-- a = b*c
-- b = a/c
instance
    ( Field a
    , SI metre3 kilogram3 second3 ampere3 kelvin3 mole3 candela3 ~ (SI metre1 kilogram1 second1 ampere1 kelvin1 mole1 candela1 >/< SI metre2 kilogram2 second2 ampere2 kelvin2 mole2 candela2)
    , SI metre1 kilogram1 second1 ampere1 kelvin1 mole1 candela1 ~ (SI metre2 kilogram2 second2 ampere2 kelvin2 mole2 candela2 >*< SI metre3 kilogram3 second3 ampere3 kelvin3 mole3 candela3)
    , SI metre2 kilogram2 second2 ampere2 kelvin2 mole2 candela2 ~ (SI metre1 kilogram1 second1 ampere1 kelvin1 mole1 candela1 >/< SI metre3 kilogram3 second3 ampere3 kelvin3 mole3 candela3)
    ) =>
    Div (SI metre1 kilogram1 second1 ampere1 kelvin1 mole1 candela1 a) (SI metre2 kilogram2 second2 ampere2 kelvin2 mole2 candela2 a) (SI metre3 kilogram3 second3 ampere3 kelvin3 mole3 candela3 a)
    where
    (>/<) (SI x) (SI y) = SI (x / y)

type Dimensionless = SI Z Z Z Z Z Z Z
dimensionless :: (Multiplicative a) => Dimensionless a
dimensionless = SI one

-- base unit
type Metre = SI P1 Z Z Z Z Z Z
metre :: (Multiplicative a) => Metre a
metre = SI one

type Meter = Metre
meter :: (Multiplicative a) => Meter a
meter = metre

type Kilogram = SI Z P1 Z Z Z Z Z
kilogram :: (Multiplicative a) => Kilogram a
kilogram = SI one

type Second = SI Z Z P1 Z Z Z Z
second :: (Multiplicative a) => Second a
second = SI one

type Ampere = SI Z Z Z P1 Z Z Z
ampere :: (Multiplicative a) => Ampere a
ampere = SI one

type Kelvin = SI Z Z Z Z P1 Z Z
kelvin :: (Multiplicative a) => Kelvin a
kelvin = SI one

type Mole = SI Z Z Z Z Z P1 Z
mole :: Multiplicative a => Mole a
mole = SI one

type Candela = SI Z Z Z Z Z Z P1
candela :: Multiplicative a => Candela a
candela = SI one

type instance Pretty (SI Z Z Z Z Z Z Z) = Dimensionless
type instance Pretty (SI Z Z Z Z Z Z P1) = Candela
type instance Pretty (SI Z Z Z Z Z P1 Z) = Mole
type instance Pretty (SI Z Z Z Z P1 Z Z) = Kelvin
type instance Pretty (SI Z Z Z P1 Z Z Z) = Ampere
type instance Pretty (SI Z Z P1 Z Z Z Z) = Second
type instance Pretty (SI Z P1 Z Z Z Z Z) = Kilogram
type instance Pretty (SI P1 Z Z Z Z Z Z) = Metre