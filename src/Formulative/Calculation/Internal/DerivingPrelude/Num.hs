module Formulative.Calculation.Internal.DerivingPrelude.Num where

import Control.Applicative
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Internal.Types
import Prelude hiding (fromInteger)
import qualified Prelude

instance (Ring a, Absolute a) => Num (MyNum a) where
    (+) (MkMyNum a) (MkMyNum b) = MkMyNum $ (.+.) a b
    (-) (MkMyNum a) (MkMyNum b) = MkMyNum $ (.-.) a b
    (*) (MkMyNum a) (MkMyNum b) = MkMyNum $ (.*.) a b
    abs (MkMyNum a) = MkMyNum $ abs' a
    signum = MkMyNum . signum' . unMkMyNum
    fromInteger = MkMyNum . fromInteger
    negate = MkMyNum . negation . unMkMyNum

instance (Ring a, Absolute a, Applicative f) => Num (MyApplicative f a) where
    (+) (MkMyApplicative a) (MkMyApplicative b) = MkMyApplicative $ liftA2 (.+.) a b
    (-) (MkMyApplicative a) (MkMyApplicative b) = MkMyApplicative $ liftA2 (.-.) a b
    (*) (MkMyApplicative a) (MkMyApplicative b) = MkMyApplicative $ liftA2 (.*.) a b
    abs = MkMyApplicative . fmap abs' . unMkMyApplicative
    signum = MkMyApplicative . fmap signum' . unMkMyApplicative
    fromInteger = MkMyApplicative . pure . fromInteger
    negate = MkMyApplicative . fmap negation . unMkMyApplicative