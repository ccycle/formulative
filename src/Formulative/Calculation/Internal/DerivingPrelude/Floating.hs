module Formulative.Calculation.Internal.DerivingPrelude.Floating where

import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Internal.DerivingPrelude.Fractional
import Formulative.Calculation.Internal.DerivingPrelude.Num
import Formulative.Calculation.Internal.Types

deriving via (MyNum a) instance (Ring a, Absolute a) => Num (MyTranscendental a)
deriving via (MyFractional a) instance (Ring a, Absolute a, Field a) => Fractional (MyTranscendental a)
deriving via (MyTranscendental a) instance (Ring a, Absolute a, Field a) => Floating (MyTranscendental a)

instance (Eq a, Absolute a, Field a, Floating a, Applicative f) => Floating (MyApplicative f a) where
    pi = MyApplicative $ pure pi
    exp = MyApplicative . fmap exp . unMyApplicative
    log = MyApplicative . fmap log . unMyApplicative
    sin = MyApplicative . fmap sin . unMyApplicative
    cos = MyApplicative . fmap cos . unMyApplicative
    asin = MyApplicative . fmap asin . unMyApplicative
    acos = MyApplicative . fmap acos . unMyApplicative
    atan = MyApplicative . fmap atan . unMyApplicative
    sinh = MyApplicative . fmap sinh . unMyApplicative
    cosh = MyApplicative . fmap cosh . unMyApplicative
    asinh = MyApplicative . fmap asinh . unMyApplicative
    acosh = MyApplicative . fmap acosh . unMyApplicative
    atanh = MyApplicative . fmap atanh . unMyApplicative
