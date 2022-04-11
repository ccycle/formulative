module Formulative.Calculation.Internal.DerivingPrelude.Floating where

import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Internal.DerivingPrelude.Fractional
import Formulative.Calculation.Internal.DerivingPrelude.Num
import Formulative.Calculation.Internal.Types

deriving via (MyNum a) instance (Ring a, Absolute a) => Num (MyTranscendental a)
deriving via (MyFractional a) instance (Ring a, Absolute a, Field a) => Fractional (MyTranscendental a)
deriving via (MyTranscendental a) instance (Ring a, Absolute a, Field a) => Floating (MyTranscendental a)

instance (Eq a, Absolute a, Field a, Floating a, Applicative f) => Floating (MyApplicative f a) where
    pi = MkMyApplicative $ pure pi
    exp = MkMyApplicative . fmap exp . unMkMyApplicative
    log = MkMyApplicative . fmap log . unMkMyApplicative
    sin = MkMyApplicative . fmap sin . unMkMyApplicative
    cos = MkMyApplicative . fmap cos . unMkMyApplicative
    asin = MkMyApplicative . fmap asin . unMkMyApplicative
    acos = MkMyApplicative . fmap acos . unMkMyApplicative
    atan = MkMyApplicative . fmap atan . unMkMyApplicative
    sinh = MkMyApplicative . fmap sinh . unMkMyApplicative
    cosh = MkMyApplicative . fmap cosh . unMkMyApplicative
    asinh = MkMyApplicative . fmap asinh . unMkMyApplicative
    acosh = MkMyApplicative . fmap acosh . unMkMyApplicative
    atanh = MkMyApplicative . fmap atanh . unMkMyApplicative
