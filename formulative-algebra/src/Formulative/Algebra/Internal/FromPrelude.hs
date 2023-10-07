module Formulative.Algebra.Internal.FromPrelude where

import GHC.Generics
import Prelude

newtype FromPrelude a = FromPrelude {unMyNumeric :: a} deriving (Show, Eq, Generic)
newtype FromPrelude1 f a = FromPrelude1 {unMyApplicative :: f a} deriving (Show, Eq, Generic)
