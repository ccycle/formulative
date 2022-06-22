module Formulative.Calculation.Algebra.Arithmetic.Algebraic where

import Formulative.Calculation.Algebra.Arithmetic.Field (Field)
import Prelude hiding (Floating (..), Num (..))
import qualified Prelude as P

import Control.Applicative
import Data.Ratio (Rational, denominator, numerator, (%))
import qualified Data.Vector.Sized as VS
import Formulative.Calculation.Algebra.Arithmetic.Multiplicative (
    Multiplicative ((.^)),
 )
import Formulative.Calculation.Internal.Type
import GHC.TypeNats

class (Field a) => Algebraic a where
    {-# MINIMAL root | (^%) #-}

    -- sqrt x  =  x ** (1/2)
    sqrt :: a -> a
    sqrt = root 2

    -- root n x  =  x ** (1/n)
    root :: P.Integer -> a -> a
    root n x = x ^% (1 % n)

    -- x ^% (n/m) = (x^n) ** (1/m)
    (^%) :: a -> Rational -> a
    x ^% y = root (denominator y) (x .^ numerator y)

instance (Eq a, P.Floating a) => Algebraic (MyNumeric a) where
    sqrt (MyNumeric x) = MyNumeric (P.sqrt x)
    root n (MyNumeric x) = MyNumeric $ x P.** P.recip (P.fromInteger n)
    (MyNumeric x) ^% y = MyNumeric $ x P.** fromRational y

deriving via (MyNumeric Double) instance Algebraic Double
deriving via (MyNumeric Float) instance Algebraic Float

instance (Algebraic a, Eq a, Applicative m, Foldable m) => Algebraic (MyApplicative m a) where
    sqrt (MyApplicative x) = MyApplicative (fmap sqrt x)
    root n (MyApplicative x) = MyApplicative $ fmap (root n) x
    (MyApplicative x) ^% y = MyApplicative $ fmap (^% y) x

deriving via (MyApplicative Maybe a) instance (Algebraic a, Eq a) => Algebraic (Maybe a)
deriving via (MyApplicative (Either a) b) instance (Algebraic b, Eq b) => Algebraic (Either a b)
deriving via (MyApplicative (VS.Vector n) a) instance (Algebraic a, Eq a, KnownNat n) => Algebraic (VS.Vector n a)