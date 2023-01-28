module Formulative.Calculation.Algebra.Arithmetic.Additive where

import Control.Algebra
import Control.Applicative
import Data.Complex
import qualified Data.Vector.Sized as VS
import Debug.SimpleReflect.Expr
import Formulative.Calculation.Internal.Types
import GHC.Generics
import GHC.Natural
import GHC.TypeNats

class Additive a where
    (.+.) :: a -> a -> a
    zero :: a

    default (.+.) :: (Generic a, GAdditive (Rep a)) => a -> a -> a
    (.+.) a b = to $ (..+..) (from a) (from b)
    default zero :: (Generic a, GAdditive (Rep a)) => a
    zero = to gZero

    infixl 6 .+.

stimesBinaryOp :: Integral b => (a -> a -> a) -> a -> b -> a -> a
stimesBinaryOp op unit y0 x0
    | y0 < 0 = error "stimes: positive multiplier expected"
    | y0 == 0 = unit
    | otherwise = f x0 y0
  where
    f x y
        | even y = f (x `op` x) (y `quot` 2)
        | y == 1 = x
        | otherwise = g (x `op` x) (y `quot` 2) x
    g x y z
        | even y = g (x `op` x) (y `quot` 2) z
        | y == 1 = x `op` z
        | otherwise = g (x `op` x) (y `quot` 2) (x `op` z)
stimesAdd n = stimesBinaryOp (.+.) zero n

class GAdditive f where
    (..+..) :: f a -> f a -> f a
    gZero :: f a
instance GAdditive a => GAdditive (M1 i c a) where
    M1 a ..+.. M1 b = M1 (a ..+.. b)
    gZero = M1 gZero
instance (GAdditive a, GAdditive b) => GAdditive (a :*: b) where
    (al :*: bl) ..+.. (ar :*: br) = (..+..) al ar :*: (..+..) bl br
    gZero = gZero :*: gZero
instance Additive a => GAdditive (K1 i a) where
    K1 a ..+.. K1 b = K1 (a .+. b)
    gZero = K1 zero

-- deriving instance: Num
instance (Num a) => Additive (MyNumeric a) where
    zero = MyNumeric 0
    (MyNumeric a) .+. (MyNumeric b) = MyNumeric (a + b)

deriving via (MyNumeric Int) instance Additive Int
deriving via (MyNumeric Integer) instance Additive Integer
deriving via (MyNumeric Natural) instance Additive Natural
deriving via (MyNumeric Word) instance Additive Word
deriving via (MyNumeric Float) instance Additive Float
deriving via (MyNumeric Double) instance Additive Double
deriving via (MyNumeric Expr) instance Additive Expr
deriving via (MyNumeric (Complex Float)) instance Additive (Complex Float)
deriving via (MyNumeric (Complex Double)) instance Additive (Complex Double)

instance (Additive a, Additive b) => Additive (a -> b) where
    (f .+. g) x = f x .+. g x
    zero x = zero
instance Additive Bool where
    zero = False
    (.+.) = (||)

-- applicative
instance (Additive a, Applicative m) => Additive (MyApplicative m a) where
    (.+.) (MyApplicative x) (MyApplicative y) = MyApplicative (liftA2 (.+.) x y)
    zero = MyApplicative (pure zero)
deriving via (MyApplicative Maybe a) instance (Additive a) => Additive (Maybe a)
deriving via (MyApplicative IO a) instance (Additive a) => Additive (IO a)
deriving via (MyApplicative (Either a) b) instance (Additive b) => Additive (Either a b)
deriving via (MyApplicative (VS.Vector n) a) instance (KnownNat n, Additive a) => Additive (VS.Vector n a)

deriving via (MyApplicative (m :: * -> *) a) instance (Additive a, Applicative m, Foldable m) => Additive (MyFoldable m a)

(|.+.|) :: (Additive a, Applicative m) => m a -> m a -> m a
(|.+.|) = liftA2 (.+.)
infixl 6 |.+.|