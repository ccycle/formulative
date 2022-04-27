{-# LANGUAGE RebindableSyntax #-}

module Formulative.Calculation.Algebra.Arithmetic.Transcendental where

-- import GHC.Generics

import qualified Data.Vector.Sized as VS
import Formulative.Calculation.Algebra.Arithmetic.Additive
import Formulative.Calculation.Algebra.Arithmetic.AdditiveGroup
import Formulative.Calculation.Algebra.Arithmetic.Field
import Formulative.Calculation.Algebra.Arithmetic.Multiplicative
import Formulative.Calculation.Algebra.Arithmetic.Ring
import Formulative.Calculation.Algebra.Arithmetic.Semiring
import Formulative.Calculation.Internal.Types
import GHC.TypeNats
import Prelude hiding (fromInteger)
import qualified Prelude

-- generalized class of Floating
class (Field a) => Transcendental a where
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

    (.**.) :: a -> a -> a
    logBase' :: a -> a -> a
    sqrt' :: a -> a
    tan' :: a -> a
    tanh' :: a -> a

    log1p' :: a -> a
    expm1' :: a -> a
    log1pexp' :: a -> a
    log1mexp' :: a -> a

    {-# INLINE (.**.) #-}
    {-# INLINE logBase' #-}
    {-# INLINE sqrt' #-}
    {-# INLINE tan' #-}
    {-# INLINE tanh' #-}
    x .**. y = exp' (log' x .*. y)
    logBase' x y = log' y ./. log' x
    sqrt' x = x .**. (1 ./. 2)
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

deriving via (MyNum a) instance (Floating a) => Additive (MyTranscendental a)
deriving via (MyNum a) instance (Floating a) => AdditiveGroup (MyTranscendental a)
deriving via (MyNum a) instance (Floating a) => Multiplicative (MyTranscendental a)
deriving via (MyNum a) instance (Floating a) => Semiring (MyTranscendental a)
deriving via (MyNum a) instance (Floating a) => Ring (MyTranscendental a)
deriving via (MyFractional a) instance (Eq a, Floating a) => Field (MyTranscendental a)

instance (Eq a, Field a, Prelude.Floating a) => Transcendental (MyTranscendental a) where
    pi' = MyTranscendental Prelude.pi
    exp' = MyTranscendental . Prelude.exp . unMyTranscendental
    log' = MyTranscendental . Prelude.log . unMyTranscendental
    sin' = MyTranscendental . Prelude.sin . unMyTranscendental
    cos' = MyTranscendental . Prelude.cos . unMyTranscendental
    asin' = MyTranscendental . Prelude.asin . unMyTranscendental
    acos' = MyTranscendental . Prelude.acos . unMyTranscendental
    atan' = MyTranscendental . Prelude.atan . unMyTranscendental
    sinh' = MyTranscendental . Prelude.sinh . unMyTranscendental
    cosh' = MyTranscendental . Prelude.cosh . unMyTranscendental
    asinh' = MyTranscendental . Prelude.asinh . unMyTranscendental
    acosh' = MyTranscendental . Prelude.acosh . unMyTranscendental
    atanh' = MyTranscendental . Prelude.atanh . unMyTranscendental

deriving via (MyTranscendental Double) instance Transcendental Double
deriving via (MyTranscendental Float) instance Transcendental Float

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