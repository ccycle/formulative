module Formulative.Algebra.Arithmetic.AdditiveGroup where

import Control.Applicative
import Data.Complex
import qualified Data.Vector.Sized as VS
import Debug.SimpleReflect.Expr
import Formulative.Algebra.Arithmetic.Additive
import Formulative.Algebra.Internal.FromPrelude
import GHC.Generics
import GHC.TypeNats
import Prelude hiding (negate, (+), (-))
import qualified Prelude as P (negate, (-))

class Additive a => AdditiveGroup a where
    (-) :: a -> a -> a
    negate :: a -> a
    (-) a b = a + negate b

    default negate :: (Generic a, GAdditiveGroup (Rep a)) => a -> a
    negate = to . gnegate . from

    infixl 6 -

-- | Generic class for Additive Group
class (GAdditive f) => GAdditiveGroup f where
    gminus :: f a -> f a -> f a
    gnegate :: f a -> f a
    a `gminus` b = a `gplus` gnegate b

instance GAdditiveGroup a => GAdditiveGroup (M1 i c a) where
    M1 a `gminus` M1 b = M1 (a `gminus` b)
    gnegate (M1 a) = M1 (gnegate a)
instance (GAdditiveGroup a, GAdditiveGroup b) => GAdditiveGroup (a :*: b) where
    (al :*: bl) `gminus` (ar :*: br) = gminus al ar :*: gminus bl br
    gnegate (al :*: bl) = gnegate al :*: gnegate bl
instance (AdditiveGroup a) => GAdditiveGroup (K1 i a) where
    K1 a `gminus` K1 b = K1 (a - b)
    gnegate (K1 a) = K1 (negate a)

----------------------------------------------------------------
-- deriving instance
----------------------------------------------------------------

instance (Num a) => AdditiveGroup (FromPrelude a) where
    negate (FromPrelude a) = FromPrelude (P.negate a)
    (FromPrelude a) - (FromPrelude b) = FromPrelude (a P.- b)

deriving via (FromPrelude Int) instance AdditiveGroup Int
deriving via (FromPrelude Integer) instance AdditiveGroup Integer
deriving via (FromPrelude Rational) instance AdditiveGroup Rational
deriving via (FromPrelude Float) instance AdditiveGroup Float
deriving via (FromPrelude Double) instance AdditiveGroup Double
deriving via (FromPrelude (Complex Float)) instance AdditiveGroup (Complex Float)
deriving via (FromPrelude (Complex Double)) instance AdditiveGroup (Complex Double)
deriving via (FromPrelude Expr) instance AdditiveGroup Expr

instance {-# OVERLAPS #-} (AdditiveGroup a, AdditiveGroup b) => AdditiveGroup (a -> b) where
    (f - g) x = f x - g x
    negate f = f . negate
instance AdditiveGroup Bool where
    negate = not

-- a - b = a + not b

-- applicative
instance (AdditiveGroup a, Applicative m) => AdditiveGroup (FromPrelude1 m a) where
    negate (FromPrelude1 a) = FromPrelude1 (fmap negate a)

deriving via (FromPrelude1 (VS.Vector n) a) instance (KnownNat n, AdditiveGroup a) => AdditiveGroup (VS.Vector n a)

integralToSign :: (Integral a, AdditiveGroup p) => a -> p -> p
integralToSign i x
    | even i = x
    | otherwise = negate x