{-# LANGUAGE RebindableSyntax #-}

module Formulative.Calculation.Algebra.Arithmetic.Transcendental where

import qualified Data.Vector.Sized as VS
import Formulative.Calculation.Algebra.Arithmetic.Additive
import Formulative.Calculation.Algebra.Arithmetic.AdditiveGroup
import Formulative.Calculation.Algebra.Arithmetic.Algebraic
import Formulative.Calculation.Algebra.Arithmetic.Field
import Formulative.Calculation.Algebra.Arithmetic.Multiplicative
import Formulative.Calculation.Algebra.Arithmetic.Ring
import Formulative.Calculation.Algebra.Arithmetic.Semiring
import Formulative.Calculation.Internal.Types
import GHC.TypeNats
import Prelude hiding (Floating (..), fromInteger)
import qualified Prelude

class (Algebraic a) => Transcendental a where
    pi' :: a
    exp' :: a -> a
    log' :: a -> a
    sin' :: a -> a
    cos' :: a -> a
    asin' :: a -> a
    acos' :: a -> a
    atan' :: a -> a
    sinh' :: a -> a
    cosh' :: a -> a
    asinh' :: a -> a
    acosh' :: a -> a
    atanh' :: a -> a

    (.**) :: a -> a -> a
    logBase' :: a -> a -> a

    tan' :: a -> a
    tanh' :: a -> a

    log1p' :: a -> a
    expm1' :: a -> a
    log1pexp' :: a -> a
    log1mexp' :: a -> a

    {-# INLINE (.**) #-}
    {-# INLINE logBase' #-}

    {-# INLINE tan' #-}
    {-# INLINE tanh' #-}
    x .** y = exp' (log' x .*. y)
    logBase' x y = log' y ./. log' x

    tan' x = sin' x ./. cos' x
    tanh' x = sinh' x ./. cosh' x

    {-# INLINE log1p' #-}
    {-# INLINE expm1' #-}
    {-# INLINE log1pexp' #-}
    {-# INLINE log1mexp' #-}
    log1p' x = log' (1 .+. x)
    expm1' x = exp' x .-. one
    log1pexp' x = log1p' (exp' x)
    log1mexp' x = log1p' (negation (exp' x))

instance (Eq a, Field a, Prelude.Floating a) => Transcendental (MyNumeric a) where
    pi' = MyNumeric Prelude.pi
    exp' = MyNumeric . Prelude.exp . unMyNumeric
    log' = MyNumeric . Prelude.log . unMyNumeric
    sin' = MyNumeric . Prelude.sin . unMyNumeric
    cos' = MyNumeric . Prelude.cos . unMyNumeric
    asin' = MyNumeric . Prelude.asin . unMyNumeric
    acos' = MyNumeric . Prelude.acos . unMyNumeric
    atan' = MyNumeric . Prelude.atan . unMyNumeric
    sinh' = MyNumeric . Prelude.sinh . unMyNumeric
    cosh' = MyNumeric . Prelude.cosh . unMyNumeric
    asinh' = MyNumeric . Prelude.asinh . unMyNumeric
    acosh' = MyNumeric . Prelude.acosh . unMyNumeric
    atanh' = MyNumeric . Prelude.atanh . unMyNumeric

deriving via (MyNumeric Double) instance Transcendental Double
deriving via (MyNumeric Float) instance Transcendental Float

instance (Transcendental a, Eq a, Applicative m, Foldable m) => Transcendental (MyApplicative m a) where
    pi' = MyApplicative $ pure pi'
    exp' = MyApplicative . fmap exp' . unMyApplicative
    log' = MyApplicative . fmap log' . unMyApplicative
    sin' = MyApplicative . fmap sin' . unMyApplicative
    cos' = MyApplicative . fmap cos' . unMyApplicative
    asin' = MyApplicative . fmap asin' . unMyApplicative
    acos' = MyApplicative . fmap acos' . unMyApplicative
    atan' = MyApplicative . fmap atan' . unMyApplicative
    sinh' = MyApplicative . fmap sinh' . unMyApplicative
    cosh' = MyApplicative . fmap cosh' . unMyApplicative
    asinh' = MyApplicative . fmap asinh' . unMyApplicative
    acosh' = MyApplicative . fmap acosh' . unMyApplicative
    atanh' = MyApplicative . fmap atanh' . unMyApplicative

deriving via (MyApplicative Maybe a) instance (Transcendental a, Eq a) => Transcendental (Maybe a)
deriving via (MyApplicative (Either a) b) instance (Transcendental b, Eq b) => Transcendental (Either a b)
deriving via (MyApplicative (VS.Vector n) a) instance (Transcendental a, Eq a, KnownNat n) => Transcendental (VS.Vector n a)