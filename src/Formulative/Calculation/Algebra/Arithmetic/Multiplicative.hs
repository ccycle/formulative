module Formulative.Calculation.Algebra.Arithmetic.Multiplicative where

import Control.Algebra
import Control.Applicative
import Data.Complex
import qualified Data.Vector.Sized as VS
import Debug.SimpleReflect.Expr (Expr)
import Formulative.Calculation.Algebra.Arithmetic.AdditiveGroup
import Formulative.Calculation.Internal.Types
import GHC.Generics
import GHC.Natural
import GHC.TypeNats
import Math.Combinatorics.Exact.Factorial (factorial)

class Multiplicative a where
    (.*.) :: a -> a -> a
    one :: a

    default (.*.) :: (Generic a, GMultiplicative (Rep a)) => a -> a -> a
    (.*.) a b = to $ (..*..) (from a) (from b)
    default one :: (Generic a, GMultiplicative (Rep a)) => a
    one = to gOne

    pow :: Integral b => b -> a -> a
    pow y0 x0
        | y0 < 0 = error "stimes: positive multiplier expected"
        | y0 == 0 = one
        | otherwise = f x0 y0
      where
        f x y
            | even y = f (x .*. x) (y `quot` 2)
            | y == 1 = x
            | otherwise = g (x .*. x) (y `quot` 2) x
        g x y z
            | even y = g (x .*. x) (y `quot` 2) z
            | y == 1 = x .*. z
            | otherwise = g (x .*. x) (y `quot` 2) (x .*. z)

    (.^) :: a -> Integer -> a
    x .^ n = pow n x

    infixl 7 .*.

    infixl 8 .^

class GMultiplicative f where
    (..*..) :: f a -> f a -> f a
    gOne :: f a
instance GMultiplicative a => GMultiplicative (M1 i c a) where
    M1 a ..*.. M1 b = M1 (a ..*.. b)
    gOne = M1 gOne
instance (GMultiplicative a, GMultiplicative b) => GMultiplicative (a :*: b) where
    (al :*: bl) ..*.. (ar :*: br) = (..*..) al ar :*: (..*..) bl br
    gOne = gOne :*: gOne
instance Multiplicative a => GMultiplicative (K1 i a) where
    K1 a ..*.. K1 b = K1 (a .*. b)
    gOne = K1 one

instance (Num a) => Multiplicative (MyNum a) where
    one = MkMyNum 1
    (MkMyNum a) .*. (MkMyNum b) = MkMyNum (a * b)

deriving via (MyNum Int) instance Multiplicative Int
deriving via (MyNum Integer) instance Multiplicative Integer
deriving via (MyNum Natural) instance Multiplicative Natural
deriving via (MyNum Word) instance Multiplicative Word
deriving via (MyNum Float) instance Multiplicative Float
deriving via (MyNum Double) instance Multiplicative Double
deriving via (MyNum (Complex Float)) instance Multiplicative (Complex Float)
deriving via (MyNum (Complex Double)) instance Multiplicative (Complex Double)
instance (Multiplicative a, Multiplicative b) => Multiplicative (a -> b) where
    (f .*. g) x = f x .*. g x
    one x = one
instance Multiplicative Bool where
    one = False
    (.*.) = (&&)

instance (Multiplicative a, Applicative m) => Multiplicative (MyApplicative m a) where
    (.*.) (MkMyApplicative a) (MkMyApplicative b) = MkMyApplicative (liftA2 (.*.) a b)
    one = MkMyApplicative (pure one)

deriving via (MyNum Expr) instance Multiplicative Expr
deriving via (MyApplicative (VS.Vector n) a) instance (KnownNat n, Multiplicative a) => Multiplicative (VS.Vector n a)
deriving via (MyApplicative Maybe a) instance (Multiplicative a) => Multiplicative (Maybe a)
deriving via (MyApplicative IO a) instance (Multiplicative a) => Multiplicative (IO a)
deriving via (MyApplicative (Either a) b) instance (Multiplicative b) => Multiplicative (Either a b)

factorialRecursive n
    | n < 0 = 1
    | n == 0 = 1
    | otherwise = n .*. factorialRecursive (n - 1)

myfactorial n
    | n <= 4 = factorialRecursive n
    | otherwise = factorial n

factorialNum :: Num a => Int -> a
factorialNum = fromIntegral . myfactorial