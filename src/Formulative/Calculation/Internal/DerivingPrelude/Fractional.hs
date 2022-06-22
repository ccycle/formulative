module Formulative.Calculation.Internal.DerivingPrelude.Fractional where

import Formulative.Calculation.Algebra.Arithmetic
import Formulative.Calculation.Internal.DerivingPrelude.Num
import Formulative.Calculation.Internal.Types

deriving via (MyNum a) instance (Ring a, Absolute a) => Num (MyFractional a)

instance (Absolute a, Field a) => Fractional (MyFractional a) where
    fromRational = MyFractional . fromRational'
    recip = MyFractional . reciprocal . unMyFractional

instance (Eq a, Field a, Absolute a, Applicative f) => Fractional (MyApplicative f a) where
    fromRational = MyApplicative . pure . fromRational'
    recip = MyApplicative . fmap reciprocal . unMyApplicative
