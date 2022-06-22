module Formulative.Calculation.Internal.DerivingPrelude.Num where

import Control.Applicative
import Formulative.Calculation.Algebra.Arithmetic
import Formulative.Calculation.Internal.Types
import Prelude hiding (fromInteger)
import qualified Prelude

instance (Ring a, Absolute a) => Num (MyNumeric a) where
    (+) (MyNumeric a) (MyNumeric b) = MyNumeric $ (.+.) a b
    (-) (MyNumeric a) (MyNumeric b) = MyNumeric $ (.-.) a b
    (*) (MyNumeric a) (MyNumeric b) = MyNumeric $ (.*.) a b
    abs (MyNumeric a) = MyNumeric $ absolute a
    signum = MyNumeric . sign . unMyNumeric
    fromInteger = MyNumeric . fromInteger
    negate = MyNumeric . negation . unMyNumeric

instance (Ring a, Absolute a, Applicative f) => Num (MyApplicative f a) where
    (+) (MyApplicative a) (MyApplicative b) = MyApplicative $ liftA2 (.+.) a b
    (-) (MyApplicative a) (MyApplicative b) = MyApplicative $ liftA2 (.-.) a b
    (*) (MyApplicative a) (MyApplicative b) = MyApplicative $ liftA2 (.*.) a b
    abs = MyApplicative . fmap absolute . unMyApplicative
    signum = MyApplicative . fmap sign . unMyApplicative
    fromInteger = MyApplicative . pure . fromInteger
    negate = MyApplicative . fmap negation . unMyApplicative