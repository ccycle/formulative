module Formulative.Calculation.Internal.Types where

import Data.Complex
import GHC.Generics

newtype MyNumeric a = MyNumeric {unMyNumeric :: a} deriving (Show, Eq, Generic)
newtype MyMatrix a = MyMatrix {unMyMatrix :: a} deriving (Show, Eq, Generic)
newtype MyApplicative f a = MyApplicative {unMyApplicative :: f a} deriving (Show, Eq, Generic)
newtype MyFunctor f a = MyFunctor {unMyFunctor :: f a} deriving (Show, Eq, Generic)
newtype MyFoldable f a = MyFoldable {unMyFoldable :: f a} deriving (Show, Eq, Generic)
