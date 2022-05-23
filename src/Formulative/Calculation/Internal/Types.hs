module Formulative.Calculation.Internal.Types where

import Data.Complex
import GHC.Generics

newtype MyNum a = MyNum {unMyNum :: a} deriving (Show, Eq, Generic)
newtype MyFractional a = MyFractional {unMyFractional :: a} deriving (Show, Eq, Generic)
newtype MyFloating a = MyFloating {unMyFloating :: a} deriving (Show, Eq, Generic)
newtype MyTranscendental a = MyTranscendental {unMyTranscendental :: a} deriving (Show, Eq, Generic)
newtype MyMatrix a = MyMatrix {unMyMatrix :: a} deriving (Show, Eq, Generic)
newtype MyApplicative f a = MyApplicative {unMyApplicative :: f a} deriving (Show, Eq, Generic)
newtype MyFunctor f a = MyFunctor {unMyFunctor :: f a} deriving (Show, Eq, Generic)
newtype MyFoldable f a = MyFoldable {unMyFoldable :: f a} deriving (Show, Eq, Generic)
