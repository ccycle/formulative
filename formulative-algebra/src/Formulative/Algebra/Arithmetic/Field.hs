{-# LANGUAGE OverloadedStrings #-}

module Formulative.Algebra.Arithmetic.Field where

import Control.Applicative
import Data.String
import Data.Typeable
import qualified Data.Vector.Sized as VS
import Debug.SimpleReflect.Expr
import Formulative.Algebra.Arithmetic.Additive
import Formulative.Algebra.Arithmetic.Multiplicative
import Formulative.Algebra.Arithmetic.Ring
import Formulative.Algebra.Internal.FromPrelude
import Formulative.Algebra.Literal.FromInteger (FromInteger)
import GHC.Generics
import GHC.TypeNats
import Refined
import Prelude hiding (Num (..), fromRational, recip, (/))
import qualified Prelude as P

class (Ring a, FromInteger a) => Field a where
    recip :: a -> a
    default recip :: (Generic a, GField (Rep a)) => a -> a
    recip = to . grecip . from

    (/) :: a -> a -> a
    x / y = x * recip y
    infixl 7 /

    fromRational :: Rational -> a
    default fromRational :: (Generic a, GField (Rep a)) => Rational -> a
    fromRational = to . gfromRational

    (^-) :: a -> Natural -> a
    x ^- n = recip (x ^+ n)

data Dividable
instance (Eq a, Additive a) => Predicate Dividable (FromPrelude a) where
    validate p (FromPrelude value)
        | value == zero = Nothing
        | otherwise = throwRefineOtherException (typeOf p) "the input value is not dividable"

instance (Eq a, Additive a, Foldable m) => Predicate Dividable (FromPrelude1 m a) where
    validate p (FromPrelude1 value)
        | zero `notElem` value = Nothing
        | otherwise = throwRefineOtherException (typeOf p) "the input value is not dividable"

safeRecip :: (Field a) => Refined Dividable a -> a
safeRecip x = recip $ unrefine x

safeDiv :: (Field a) => a -> Refined Dividable a -> a
safeDiv x y = x / unrefine y

-- | Generic class for Field
class (GMultiplicative f) => GField f where
    gdiv :: f a -> f a -> f a
    grecip :: f a -> f a
    a `gdiv` b = a `gmultiply` grecip b
    gfromRational :: Rational -> f a

instance GField a => GField (M1 i c a) where
    (M1 a) `gdiv` (M1 b) = M1 (a `gdiv` b)
    grecip (M1 a) = M1 (grecip a)
    gfromRational a = M1 (gfromRational a)
instance (GField a, GField b) => GField (a :*: b) where
    (al :*: bl) `gdiv` (ar :*: br) = gdiv al ar :*: gdiv bl br
    grecip (al :*: bl) = grecip al :*: grecip bl
    gfromRational a = gfromRational a :*: gfromRational a
instance (Field a) => GField (K1 i a) where
    K1 a `gdiv` (K1 b) = K1 (a / b)
    grecip (K1 a) = K1 (recip a)
    gfromRational a = K1 (fromRational a)

instance (Eq a, P.Fractional a) => Field (FromPrelude a) where
    recip (FromPrelude a) = FromPrelude (P.recip a)
    (FromPrelude a) / (FromPrelude b) = FromPrelude (a P./ b)
    fromRational = FromPrelude . P.fromRational

deriving via (FromPrelude Rational) instance Field Rational
deriving via (FromPrelude Double) instance Field Double
deriving via (FromPrelude Float) instance Field Float
deriving via (FromPrelude Expr) instance Field Expr

-- applicative
instance (Eq a, Field a, Applicative m, Foldable m) => Field (FromPrelude1 m a) where
    recip (FromPrelude1 x) = FromPrelude1 $ fmap recip x
    fromRational = FromPrelude1 . pure . fromRational

deriving via (FromPrelude1 (VS.Vector n) a) instance (Eq a, KnownNat n, Field a) => Field (VS.Vector n a)
