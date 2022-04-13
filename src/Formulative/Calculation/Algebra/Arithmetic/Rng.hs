{-# LANGUAGE RebindableSyntax #-}

module Formulative.Calculation.Algebra.Arithmetic.Rng where

-- module Formulative.Calculation.Algebra.Arithmetic.Rng where

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

class (Additive a, Multiplicative a) => Rng a where
    fromInteger :: Integer -> a
    default fromInteger :: (Generic a, GRng (Rep a)) => Integer -> a
    fromInteger = to . gfromInteger

class (GMultiplicative f, GAdditive f) => GRng f where
    gfromInteger :: Integer -> f a
instance GRng a => GRng (M1 i c a) where
    gfromInteger a = M1 (gfromInteger a)
instance (GRng a, GRng b) => GRng (a :*: b) where
    gfromInteger a = gfromInteger a :*: gfromInteger a
instance (Rng a) => GRng (K1 i a) where
    gfromInteger a = K1 (fromInteger a)

-- deriving instance
instance (Num a) => Rng (MyNum a) where
    fromInteger = MyNum . Prelude.fromInteger

instance (Rng a, Applicative m) => Rng (MyApplicative m a) where
    fromInteger = MyApplicative . pure . fromInteger

deriving via (MyNum Word) instance Rng Word
deriving via (MyNum Int) instance Rng Int
deriving via (MyNum Integer) instance Rng Integer
deriving via (MyNum Natural) instance Rng Natural
deriving via (MyNum Float) instance Rng Float
deriving via (MyNum Double) instance Rng Double
deriving via (MyNum Expr) instance Rng Expr

deriving via (MyApplicative IO a) instance (Rng a) => Rng (IO a)
deriving via (MyApplicative Maybe a) instance (Rng a) => Rng (Maybe a)
deriving via (MyApplicative (Either a) b) instance (Rng b) => Rng (Either a b)
deriving via (MyApplicative (VS.Vector n) a) instance (KnownNat n, Rng a) => Rng (VS.Vector n a)