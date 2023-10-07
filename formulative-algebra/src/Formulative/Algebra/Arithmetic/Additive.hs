module Formulative.Algebra.Arithmetic.Additive where

import Control.Applicative
import Data.Complex
import Data.String
import qualified Data.Vector.Sized as VS
import Debug.SimpleReflect.Expr
import Formulative.Algebra.Internal.FromPrelude
import GHC.Generics
import GHC.Natural
import GHC.TypeNats
import Prelude hiding ((+))
import qualified Prelude as P

class Additive a where
    (+) :: a -> a -> a
    zero :: a

    default (+) :: (Generic a, GAdditive (Rep a)) => a -> a -> a
    (+) a b = to $ gplus (from a) (from b)
    default zero :: (Generic a, GAdditive (Rep a)) => a
    zero = to gzero

    infixl 6 +

stimesBinaryOp :: (a -> a -> a) -> a -> Natural -> a -> a
stimesBinaryOp operator unit y0 x0
    | y0 < 0 = error "stimes: positive multiplier expected"
    | y0 == 0 = unit
    | otherwise = f x0 y0
  where
    f x y
        | even y = f (x `operator` x) (y `quot` 2)
        | y == 1 = x
        | otherwise = g (x `operator` x) (y `quot` 2) x
    g x y z
        | even y = g (x `operator` x) (y `quot` 2) z
        | y == 1 = x `operator` z
        | otherwise = g (x `operator` x) (y `quot` 2) (x `operator` z)

stimesAdd :: Additive a => Natural -> a -> a
stimesAdd = stimesBinaryOp (+) zero

class GAdditive f where
    gplus :: f a -> f a -> f a
    gzero :: f a
instance GAdditive a => GAdditive (M1 i c a) where
    M1 a `gplus` M1 b = M1 (a `gplus` b)
    gzero = M1 gzero
instance (GAdditive a, GAdditive b) => GAdditive (a :*: b) where
    (al :*: bl) `gplus` (ar :*: br) = gplus al ar :*: gplus bl br
    gzero = gzero :*: gzero
instance Additive a => GAdditive (K1 i a) where
    K1 a `gplus` K1 b = K1 (a + b)
    gzero = K1 zero

-- deriving instance: Num
instance (Num a) => Additive (FromPrelude a) where
    zero = FromPrelude 0
    (FromPrelude a) + (FromPrelude b) = FromPrelude (a P.+ b)

deriving via (FromPrelude Int) instance Additive Int
deriving via (FromPrelude Integer) instance Additive Integer
deriving via (FromPrelude Rational) instance Additive Rational
deriving via (FromPrelude Word) instance Additive Word
deriving via (FromPrelude Natural) instance Additive Natural
deriving via (FromPrelude Float) instance Additive Float
deriving via (FromPrelude Double) instance Additive Double
deriving via (FromPrelude Expr) instance Additive Expr
deriving via (FromPrelude (Complex Float)) instance Additive (Complex Float)
deriving via (FromPrelude (Complex Double)) instance Additive (Complex Double)

instance (Additive b) => Additive (a -> b) where
    (f + g) x = f x + g x
    zero _ = zero
instance Additive Bool where
    zero = False
    (+) = (||)

-- applicative
instance (Additive a, Applicative m) => Additive (FromPrelude1 m a) where
    (+) (FromPrelude1 x) (FromPrelude1 y) = FromPrelude1 (liftA2 (+) x y)
    zero = FromPrelude1 (pure zero)

deriving via (FromPrelude1 (VS.Vector n) a) instance (KnownNat n, Additive a) => Additive (VS.Vector n a)
