module Formulative.Calculation.Internal.DerivingPrelude.Fractional where

import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Internal.DerivingPrelude.Num
import Formulative.Calculation.Internal.Types

deriving via (MyNum a) instance (Ring a, Absolute a) => Num (MyFractional a)

instance (Absolute a, Field a) => Fractional (MyFractional a) where
    fromRational = MkMyFractional . fromRational'
    recip = MkMyFractional . reciprocal . unMkMyFractional

instance (Eq a, Field a, Absolute a, Applicative f) => Fractional (MyApplicative f a) where
    fromRational = MkMyApplicative . pure . fromRational'
    recip = MkMyApplicative . fmap reciprocal . unMkMyApplicative
