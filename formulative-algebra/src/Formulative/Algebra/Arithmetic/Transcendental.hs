module Formulative.Algebra.Arithmetic.Transcendental where

import qualified Data.Vector.Sized as VS
import Formulative.Algebra.Arithmetic.Additive
import Formulative.Algebra.Arithmetic.AdditiveGroup
import Formulative.Algebra.Arithmetic.Algebraic
import Formulative.Algebra.Arithmetic.Field
import Formulative.Algebra.Arithmetic.Multiplicative
import Formulative.Algebra.Internal.FromPrelude
import Formulative.Algebra.Literal.FromInteger
import GHC.TypeNats
import Prelude hiding (Floating (..), Num (..), fromInteger, negate, (/))
import qualified Prelude as P

class (Algebraic a) => Transcendental a where
    pi :: a
    exp :: a -> a
    log :: a -> a
    sin :: a -> a
    cos :: a -> a
    asin :: a -> a
    acos :: a -> a
    atan :: a -> a
    sinh :: a -> a
    cosh :: a -> a
    asinh :: a -> a
    acosh :: a -> a
    atanh :: a -> a

    (**) :: a -> a -> a
    logBase :: a -> a -> a

    tan :: a -> a
    tanh :: a -> a

    log1p :: a -> a
    expm1 :: a -> a
    log1pexp :: a -> a
    log1mexp :: a -> a

    {-# INLINE (**) #-}
    {-# INLINE logBase #-}

    {-# INLINE tan #-}
    {-# INLINE tanh #-}
    x ** y = exp (log x * y)
    logBase x y = log y / log x

    tan x = sin x / cos x
    tanh x = sinh x / cosh x

    {-# INLINE log1p #-}
    {-# INLINE expm1 #-}
    {-# INLINE log1pexp #-}
    {-# INLINE log1mexp #-}
    log1p x = log (1 + x)
    expm1 x = exp x - one
    log1pexp x = log1p (exp x)
    log1mexp x = log1p (negate (exp x))

instance (Eq a, Field a, P.Floating a) => Transcendental (FromPrelude a) where
    pi = FromPrelude P.pi
    exp = FromPrelude . P.exp . unMyNumeric
    log = FromPrelude . P.log . unMyNumeric
    sin = FromPrelude . P.sin . unMyNumeric
    cos = FromPrelude . P.cos . unMyNumeric
    asin = FromPrelude . P.asin . unMyNumeric
    acos = FromPrelude . P.acos . unMyNumeric
    atan = FromPrelude . P.atan . unMyNumeric
    sinh = FromPrelude . P.sinh . unMyNumeric
    cosh = FromPrelude . P.cosh . unMyNumeric
    asinh = FromPrelude . P.asinh . unMyNumeric
    acosh = FromPrelude . P.acosh . unMyNumeric
    atanh = FromPrelude . P.atanh . unMyNumeric

deriving via (FromPrelude Double) instance Transcendental Double
deriving via (FromPrelude Float) instance Transcendental Float

instance (Transcendental a, Eq a, Applicative m, Foldable m) => Transcendental (FromPrelude1 m a) where
    pi = FromPrelude1 $ pure pi
    exp = FromPrelude1 . fmap exp . unMyApplicative
    log = FromPrelude1 . fmap log . unMyApplicative
    sin = FromPrelude1 . fmap sin . unMyApplicative
    cos = FromPrelude1 . fmap cos . unMyApplicative
    asin = FromPrelude1 . fmap asin . unMyApplicative
    acos = FromPrelude1 . fmap acos . unMyApplicative
    atan = FromPrelude1 . fmap atan . unMyApplicative
    sinh = FromPrelude1 . fmap sinh . unMyApplicative
    cosh = FromPrelude1 . fmap cosh . unMyApplicative
    asinh = FromPrelude1 . fmap asinh . unMyApplicative
    acosh = FromPrelude1 . fmap acosh . unMyApplicative
    atanh = FromPrelude1 . fmap atanh . unMyApplicative

-- deriving via (FromPrelude1 Maybe a) instance (Transcendental a, Eq a) => Transcendental (Maybe a)
-- deriving via (FromPrelude1 (Either a) b) instance (Transcendental b, Eq b) => Transcendental (Either a b)
deriving via (FromPrelude1 (VS.Vector n) a) instance (Transcendental a, Eq a, KnownNat n) => Transcendental (VS.Vector n a)