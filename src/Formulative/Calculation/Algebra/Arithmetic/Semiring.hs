module Formulative.Calculation.Algebra.Arithmetic.Semiring where

import qualified Data.Vector.Sized as VS
import Debug.SimpleReflect.Expr
import Formulative.Calculation.Algebra.Arithmetic.Additive
import Formulative.Calculation.Algebra.Arithmetic.Multiplicative
import Formulative.Calculation.Internal.Types
import GHC.Generics
import GHC.Natural
import GHC.TypeNats
import Prelude hiding (fromInteger)
import qualified Prelude

class (Additive a, Multiplicative a) => Semiring a where
    fromInteger :: Integer -> a
    default fromInteger :: (Generic a, GSemiring (Rep a)) => Integer -> a
    fromInteger = to . gfromInteger

class (GMultiplicative f, GAdditive f) => GSemiring f where
    gfromInteger :: Integer -> f a
instance GSemiring a => GSemiring (M1 i c a) where
    gfromInteger a = M1 (gfromInteger a)
instance (GSemiring a, GSemiring b) => GSemiring (a :*: b) where
    gfromInteger a = gfromInteger a :*: gfromInteger a
instance (Semiring a) => GSemiring (K1 i a) where
    gfromInteger a = K1 (fromInteger a)

instance (Num a) => Semiring (MyNumeric a) where
    fromInteger = MyNumeric . Prelude.fromInteger

instance (Semiring a, Applicative m) => Semiring (MyApplicative m a) where
    fromInteger = MyApplicative . pure . fromInteger

deriving via (MyNumeric Word) instance Semiring Word
deriving via (MyNumeric Int) instance Semiring Int
deriving via (MyNumeric Integer) instance Semiring Integer
deriving via (MyNumeric Natural) instance Semiring Natural
deriving via (MyNumeric Float) instance Semiring Float
deriving via (MyNumeric Double) instance Semiring Double
deriving via (MyNumeric Expr) instance Semiring Expr

deriving via (MyApplicative IO a) instance (Semiring a) => Semiring (IO a)
deriving via (MyApplicative Maybe a) instance (Semiring a) => Semiring (Maybe a)
deriving via (MyApplicative (Either a) b) instance (Semiring b) => Semiring (Either a b)
deriving via (MyApplicative (VS.Vector n) a) instance (KnownNat n, Semiring a) => Semiring (VS.Vector n a)