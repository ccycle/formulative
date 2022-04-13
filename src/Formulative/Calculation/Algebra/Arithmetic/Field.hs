{-# LANGUAGE OverloadedStrings #-}

module Formulative.Calculation.Algebra.Arithmetic.Field where

import Control.Algebra
import Control.Applicative
import qualified Data.Text as T
import Data.Typeable
import qualified Data.Vector.Sized as VS
import Debug.SimpleReflect.Expr
import Formulative.Calculation.Algebra.Arithmetic.Additive
import Formulative.Calculation.Algebra.Arithmetic.AdditiveGroup
import Formulative.Calculation.Algebra.Arithmetic.Multiplicative
import Formulative.Calculation.Algebra.Arithmetic.Ring
import Formulative.Calculation.Algebra.Arithmetic.Rng
import Formulative.Calculation.Internal.Types
import GHC.Generics
import GHC.TypeNats
import Refined
import Refined.Unsafe.Type

class (Ring a) => Field a where
    reciprocal :: a -> a
    fromRational' :: Rational -> a
    isDividable :: a -> Bool

    default reciprocal :: (Generic a, GField (Rep a)) => a -> a
    reciprocal = to . gReciprocal . from
    (./.) :: a -> a -> a
    a ./. b = a .*. reciprocal b

    default fromRational' :: (Generic a, GField (Rep a)) => Rational -> a
    fromRational' = to . gFromRational'

    default isDividable :: (Eq a, Generic a, GField (Rep a)) => a -> Bool
    isDividable a = zero == a

    infixl 7 ./.

data Dividable
instance (Field a, Eq a) => Predicate Dividable a where
    validate p value =
        if isDividable value
            then Nothing
            else throwRefineOtherException (typeOf p) "the input value includes zero"
instance Functor (Refined Dividable) where
    fmap f (Refined a) = Refined $ f a

safeRecip :: (Field a) => Refined Dividable a -> a
safeRecip x = reciprocal $ unrefine x

safeDiv :: (Field a) => a -> Refined Dividable a -> a
safeDiv x y = x ./. unrefine y

-- | Generic class for Field
class (GMultiplicative f) => GField f where
    (../..) :: f a -> f a -> f a
    gReciprocal :: f a -> f a
    a ../.. b = a ..*.. gReciprocal b
    gFromRational' :: Rational -> f a
    gIsDividable :: f a -> Bool

instance GField a => GField (M1 i c a) where
    (M1 a) ../.. (M1 b) = M1 (a ../.. b)
    gReciprocal (M1 a) = M1 (gReciprocal a)
    gFromRational' a = M1 (gFromRational' a)
    gIsDividable (M1 a) = gIsDividable a
instance (GField a, GField b) => GField (a :*: b) where
    (al :*: bl) ../.. (ar :*: br) = (../..) al ar :*: (../..) bl br
    gReciprocal (al :*: bl) = gReciprocal al :*: gReciprocal bl
    gFromRational' a = gFromRational' a :*: gFromRational' a
    gIsDividable (al :*: bl) = gIsDividable al && gIsDividable bl
instance (Field a) => GField (K1 i a) where
    K1 a ../.. (K1 b) = K1 (a ./. b)
    gReciprocal (K1 a) = K1 (reciprocal a)
    gFromRational' a = K1 (fromRational' a)
    gIsDividable (K1 a) = isDividable a

-- deriving instance
-- Num
deriving via (MyNum a) instance (Fractional a) => Additive (MyFractional a)
deriving via (MyNum a) instance (Fractional a) => AdditiveGroup (MyFractional a)
deriving via (MyNum a) instance (Fractional a) => Multiplicative (MyFractional a)
deriving via (MyNum a) instance (Fractional a) => Rng (MyFractional a)
deriving via (MyNum a) instance (Fractional a) => Ring (MyFractional a)
instance (Eq a, Fractional a) => Field (MyFractional a) where
    reciprocal (MyFractional a) = MyFractional (recip a)
    (MyFractional a) ./. (MyFractional b) = MyFractional (a / b)
    fromRational' = MyFractional . fromRational
    isDividable x = zero /= x

deriving via (MyFractional Double) instance Field Double
deriving via (MyFractional Float) instance Field Float
deriving via (MyFractional Expr) instance Field Expr

-- applicative
instance (Eq a, Field a, Applicative m, Foldable m) => Field (MyApplicative m a) where
    reciprocal (MyApplicative x) = MyApplicative $ fmap reciprocal x
    fromRational' = MyApplicative . pure . fromRational'
    isDividable (MyApplicative x) = zero `notElem` x

-- - isDividableが定義できないのでIOの場合はFieldのインスタンスにはなれない
--   - dividableかどうかが毎回実行時に変わるため

deriving via (MyApplicative Maybe a) instance (Eq a, Field a) => Field (Maybe a)
deriving via (MyApplicative (Either a) b) instance (Eq b, Field b) => Field ((Either a) b)
deriving via (MyApplicative (VS.Vector n) a) instance (Eq a, KnownNat n, Field a) => Field (VS.Vector n a)

-- 足し引きとelementwise map(fmap)のみが使える型に対して要素間の積を計算
mult :: (AdditiveGroup (m a), Field a) => ((a -> a) -> m a -> m a) -> m a -> m a -> m a
mult mapFromLibrary x y = mapFromLibrary ((fromRational' $ 1 / 2) .*.) (mapFromLibrary (.^ 2) (x .+. y) .-. mapFromLibrary (.^ 2) x .-. mapFromLibrary (.^ 2) y)

(<./.>) :: forall sig m a. (Algebra sig m, Field a) => m a -> m a -> m a
(<./.>) = liftA2 (./.)