module Formulative.Calculation.Internal.Types where

import Data.Complex
import GHC.Generics

newtype MyNum a = MkMyNum {unMkMyNum :: a} deriving (Show, Eq, Generic)
newtype MyComplex a = MkMyComplex {unMkMyComplex :: Complex a} deriving (Show, Eq, Generic)
newtype MyApplicative f a = MkMyApplicative {unMkMyApplicative :: f a} deriving (Show, Eq, Generic)
newtype MyFunctor f a = MkMyFunctor {unMkMyFunctor :: f a} deriving (Show, Eq, Generic)
newtype MyFoldable f a = MkMyFoldable {unMkMyFoldable :: f a} deriving (Show, Eq, Generic)
newtype MyFractional a = MkMyFractional {unMkMyFractional :: a} deriving (Show, Eq, Generic)
newtype MyFloating a = MkMyFloating {unMkMyFloating :: a} deriving (Show, Eq, Generic)
newtype MyTranscendental a = MyTranscendental {unMyTranscendental :: a} deriving (Show, Eq, Generic)
