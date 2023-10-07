module Formulative.Algebra.Arithmetic.Semiring where

import qualified Data.Vector.Sized as VS
import Debug.SimpleReflect.Expr
import Formulative.Algebra.Arithmetic.Additive
import Formulative.Algebra.Arithmetic.Multiplicative
import Formulative.Algebra.Internal.FromPrelude
import GHC.Natural
import GHC.TypeNats
import Prelude hiding (fromInteger, (*))

class (Additive a, Multiplicative a) => Semiring a

instance (Semiring b) => Semiring (a -> b)

instance (Num a) => Semiring (FromPrelude a)

instance (Semiring a, Applicative m) => Semiring (FromPrelude1 m a)

deriving via (FromPrelude Word) instance Semiring Word
deriving via (FromPrelude Int) instance Semiring Int
deriving via (FromPrelude Integer) instance Semiring Integer
deriving via (FromPrelude Rational) instance Semiring Rational
deriving via (FromPrelude Natural) instance Semiring Natural
deriving via (FromPrelude Float) instance Semiring Float
deriving via (FromPrelude Double) instance Semiring Double
deriving via (FromPrelude Expr) instance Semiring Expr

deriving via (FromPrelude1 (VS.Vector n) a) instance (KnownNat n, Semiring a) => Semiring (VS.Vector n a)