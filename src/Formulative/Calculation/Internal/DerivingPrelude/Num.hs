module Formulative.Calculation.Internal.DerivingPrelude.Num where

import Control.Applicative
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Internal.Types
import Prelude hiding (fromInteger)
import qualified Prelude

instance (Ring a, Absolute a) => Num (MyNum a) where
    (+) (MyNum a) (MyNum b) = MyNum $ (.+.) a b
    (-) (MyNum a) (MyNum b) = MyNum $ (.-.) a b
    (*) (MyNum a) (MyNum b) = MyNum $ (.*.) a b
    abs (MyNum a) = MyNum $ abs' a
    signum = MyNum . signum' . unMyNum
    fromInteger = MyNum . fromInteger
    negate = MyNum . negation . unMyNum

instance (Ring a, Absolute a, Applicative f) => Num (MyApplicative f a) where
    (+) (MyApplicative a) (MyApplicative b) = MyApplicative $ liftA2 (.+.) a b
    (-) (MyApplicative a) (MyApplicative b) = MyApplicative $ liftA2 (.-.) a b
    (*) (MyApplicative a) (MyApplicative b) = MyApplicative $ liftA2 (.*.) a b
    abs = MyApplicative . fmap abs' . unMyApplicative
    signum = MyApplicative . fmap signum' . unMyApplicative
    fromInteger = MyApplicative . pure . fromInteger
    negate = MyApplicative . fmap negation . unMyApplicative