{-# LANGUAGE RebindableSyntax #-}

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
    default fromInteger :: (Generic a, GRng (Rep a)) => Integer -> a
    fromInteger = to . gfromInteger

class (GMultiplicative f, GAdditive f) => GRng f where
    gfromInteger :: Integer -> f a
instance GRng a => GRng (M1 i c a) where
    gfromInteger a = M1 (gfromInteger a)
instance (GRng a, GRng b) => GRng (a :*: b) where
    gfromInteger a = gfromInteger a :*: gfromInteger a
instance (Semiring a) => GRng (K1 i a) where
    gfromInteger a = K1 (fromInteger a)

instance (Num a) => Semiring (MyNum a) where
    fromInteger = MyNum . Prelude.fromInteger

instance (Semiring a, Applicative m) => Semiring (MyApplicative m a) where
    fromInteger = MyApplicative . pure . fromInteger

deriving via (MyNum Word) instance Semiring Word
deriving via (MyNum Int) instance Semiring Int
deriving via (MyNum Integer) instance Semiring Integer
deriving via (MyNum Natural) instance Semiring Natural
deriving via (MyNum Float) instance Semiring Float
deriving via (MyNum Double) instance Semiring Double
deriving via (MyNum Expr) instance Semiring Expr

deriving via (MyApplicative IO a) instance (Semiring a) => Semiring (IO a)
deriving via (MyApplicative Maybe a) instance (Semiring a) => Semiring (Maybe a)
deriving via (MyApplicative (Either a) b) instance (Semiring b) => Semiring (Either a b)
deriving via (MyApplicative (VS.Vector n) a) instance (KnownNat n, Semiring a) => Semiring (VS.Vector n a)