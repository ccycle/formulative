module Formulative.Algebra.Arithmetic.Algebraic (
    Algebraic (..),
    (^),
) where

import qualified Data.Vector.Sized as VS
import Formulative.Algebra.Arithmetic.Field
import Formulative.Algebra.Arithmetic.Multiplicative (
    Multiplicative ((^+)),
 )
import Formulative.Algebra.Internal.FromPrelude
import Formulative.Algebra.Literal.FromInteger
import GHC.Integer (absInteger)
import GHC.Real (Ratio (..), (%))
import GHC.TypeNats
import Prelude (Floating)
import Prelude hiding (Floating (..), Num (..), fromRational, (^))
import qualified Prelude as P

data Sign = Positive | Negative
data SignedNatural = SignedNatural Sign Natural

integerToSignedNatural :: Integer -> SignedNatural
integerToSignedNatural n
    | n < 0 = SignedNatural Negative (fromInteger (absInteger n))
    | otherwise = SignedNatural Positive (fromIntegral n)

(^) :: Field a => a -> Integer -> a
x ^ n = case integerToSignedNatural n of
    SignedNatural Positive m -> x ^+ m
    SignedNatural Negative m -> x ^- m

class (Field a, FromInteger a) => Algebraic a where
    {-# MINIMAL root | (^%) #-}

    sqrt :: a -> a
    sqrt = root 2

    root :: Integer -> a -> a
    root n x = x ^% (1 % n)

    (^%) :: a -> Rational -> a
    x ^% (n :% m) = root n (x ^ m)

instance (Eq a, Floating a) => Algebraic (FromPrelude a) where
    sqrt (FromPrelude x) = FromPrelude (P.sqrt x)
    root n (FromPrelude x) = FromPrelude $ x P.** P.recip (P.fromInteger n)
    (FromPrelude x) ^% y = FromPrelude $ x P.** P.fromRational y

deriving via (FromPrelude Double) instance Algebraic Double
deriving via (FromPrelude Float) instance Algebraic Float

instance (Algebraic a, Eq a, Applicative m, Foldable m) => Algebraic (FromPrelude1 m a) where
    sqrt (FromPrelude1 x) = FromPrelude1 (fmap sqrt x)
    root n (FromPrelude1 x) = FromPrelude1 $ fmap (root n) x
    (FromPrelude1 x) ^% y = FromPrelude1 $ fmap (^% y) x

deriving via (FromPrelude1 (VS.Vector n) a) instance (Algebraic a, Eq a, KnownNat n) => Algebraic (VS.Vector n a)