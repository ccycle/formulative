module Formulative.Algebra.Arithmetic.Multiplicative where

import Control.Applicative
import Data.Complex
import Data.String
import qualified Data.Vector.Sized as VS
import Data.Word
import Debug.SimpleReflect.Expr (Expr)
import Formulative.Algebra.Arithmetic.Additive
import Formulative.Algebra.Arithmetic.AdditiveGroup
import Formulative.Algebra.Internal.FromPrelude
import GHC.Generics
import GHC.Natural
import GHC.TypeNats
import Prelude (Num, fromInteger)
import Prelude hiding (Num (..), negate)
import qualified Prelude as P

class Multiplicative a where
    (*) :: a -> a -> a
    one :: a

    default (*) :: (Generic a, GMultiplicative (Rep a)) => a -> a -> a
    (*) a b = to $ gmultiply (from a) (from b)
    default one :: (Generic a, GMultiplicative (Rep a)) => a
    one = to gOne

    pow :: Natural -> a -> a
    pow y0 x0
        | y0 < 0 = error "stimes: positive multiplier expected"
        | y0 == 0 = one
        | otherwise = f x0 y0
      where
        f x y
            | even y = f (x * x) (y `quot` 2)
            | y == 1 = x
            | otherwise = g (x * x) (y `quot` 2) x
        g x y z
            | even y = g (x * x) (y `quot` 2) z
            | y == 1 = x * z
            | otherwise = g (x * x) (y `quot` 2) (x * z)

    (^+) :: a -> Natural -> a
    x ^+ n = pow n x

    infixl 7 *

    infixl 8 ^+

class GMultiplicative f where
    gmultiply :: f a -> f a -> f a
    gOne :: f a
instance GMultiplicative a => GMultiplicative (M1 i c a) where
    M1 a `gmultiply` M1 b = M1 (a `gmultiply` b)
    gOne = M1 gOne
instance (GMultiplicative a, GMultiplicative b) => GMultiplicative (a :*: b) where
    (al :*: bl) `gmultiply` (ar :*: br) = gmultiply al ar :*: gmultiply bl br
    gOne = gOne :*: gOne
instance Multiplicative a => GMultiplicative (K1 i a) where
    K1 a `gmultiply` K1 b = K1 (a * b)
    gOne = K1 one

instance Multiplicative b => Multiplicative (a -> b) where
    (*) f g x = f x * g x
    one _ = one

instance (Num a) => Multiplicative (FromPrelude a) where
    one = FromPrelude 1
    (FromPrelude a) * (FromPrelude b) = FromPrelude (a P.* b)

deriving via (FromPrelude Int) instance Multiplicative Int
deriving via (FromPrelude Integer) instance Multiplicative Integer
deriving via (FromPrelude Rational) instance Multiplicative Rational
deriving via (FromPrelude Natural) instance Multiplicative Natural
deriving via (FromPrelude Word) instance Multiplicative Word
deriving via (FromPrelude Word16) instance Multiplicative Word16
deriving via (FromPrelude Word32) instance Multiplicative Word32
deriving via (FromPrelude Word64) instance Multiplicative Word64
deriving via (FromPrelude Float) instance Multiplicative Float
deriving via (FromPrelude Double) instance Multiplicative Double
deriving via (FromPrelude (Complex Float)) instance Multiplicative (Complex Float)
deriving via (FromPrelude (Complex Double)) instance Multiplicative (Complex Double)

instance Multiplicative Bool where
    one = False
    (*) = (&&)

instance (Multiplicative a, Applicative m) => Multiplicative (FromPrelude1 m a) where
    (*) (FromPrelude1 a) (FromPrelude1 b) = FromPrelude1 (liftA2 (*) a b)
    one = FromPrelude1 (pure one)

deriving via (FromPrelude Expr) instance Multiplicative Expr
deriving via (FromPrelude1 (VS.Vector n) a) instance (KnownNat n, Multiplicative a) => Multiplicative (VS.Vector n a)
deriving via (FromPrelude1 Maybe a) instance (Multiplicative a) => Multiplicative (Maybe a)
deriving via (FromPrelude1 IO a) instance (Multiplicative a) => Multiplicative (IO a)
deriving via (FromPrelude1 (Either a) b) instance (Multiplicative b) => Multiplicative (Either a b)

factorial :: (Ord t, AdditiveGroup t, Multiplicative t) => t -> t
factorial n = go n one
  where
    go n1 n2
        | n1 <= zero = n2
        | otherwise = go (n1 - one) (n1 * n2)
