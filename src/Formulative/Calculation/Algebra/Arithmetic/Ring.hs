{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.Algebra.Arithmetic.Ring where

import qualified Data.Vector.Sized as VS
import Debug.SimpleReflect.Expr
import Formulative.Calculation.Algebra.Arithmetic.Additive
import Formulative.Calculation.Algebra.Arithmetic.AdditiveGroup
import Formulative.Calculation.Algebra.Arithmetic.Multiplicative
import Formulative.Calculation.Algebra.Arithmetic.Semiring (Semiring)
import Formulative.Calculation.Internal.Types
import GHC.Generics
import GHC.TypeNats
import Prelude hiding (fromInteger)
import qualified Prelude

-- class (AdditiveGroup a, Multiplicative a) => Ring a
class (AdditiveGroup a, Semiring a) => Ring a

-- fromInteger :: Integer -> a
-- default fromInteger :: (Generic a, GRing (Rep a)) => Integer -> a
-- fromInteger = to . gfromInteger

-- class (GMultiplicative f, GAdditiveGroup f) => GRing f where
--     gfromInteger :: Integer -> f a
-- instance GRing a => GRing (M1 i c a) where
--     gfromInteger a = M1 (gfromInteger a)
-- instance (GRing a, GRing b) => GRing (a :*: b) where
--     gfromInteger a = gfromInteger a :*: gfromInteger a
-- instance (Ring a) => GRing (K1 i a) where
--     gfromInteger a = K1 (fromInteger a)

-- -- deriving instance
instance (Num a) => Ring (MyNumeric a)

-- fromInteger = MyNumeric . Prelude.fromInteger

-- fromInteger = MyApplicative . pure . fromInteger

deriving via (MyNumeric Word) instance Ring Word
deriving via (MyNumeric Int) instance Ring Int
deriving via (MyNumeric Integer) instance Ring Integer
deriving via (MyNumeric Float) instance Ring Float
deriving via (MyNumeric Double) instance Ring Double
deriving via (MyNumeric Expr) instance Ring Expr

instance (Ring a, Applicative m) => Ring (MyApplicative m a)
deriving via (MyApplicative IO a) instance (Ring a) => Ring (IO a)
deriving via (MyApplicative Maybe a) instance (Ring a) => Ring (Maybe a)
deriving via (MyApplicative (Either a) b) instance (Ring b) => Ring (Either a b)
deriving via (MyApplicative (VS.Vector n) a) instance (KnownNat n, Ring a) => Ring (VS.Vector n a)