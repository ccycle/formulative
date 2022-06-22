module Formulative.Calculation.Internal.DerivingPrelude.Fractional where

import Formulative.Calculation.Algebra.Arithmetic
import Formulative.Calculation.Internal.DerivingPrelude.Num
import Formulative.Calculation.Internal.Types

instance (Absolute a, Field a) => Fractional (MyNumeric a) where
    fromRational = MyNumeric . fromRational'
    recip = MyNumeric . reciprocal . unMyNumeric

instance (Eq a, Field a, Absolute a, Applicative f) => Fractional (MyApplicative f a) where
    fromRational = MyApplicative . pure . fromRational'
    recip = MyApplicative . fmap reciprocal . unMyApplicative
