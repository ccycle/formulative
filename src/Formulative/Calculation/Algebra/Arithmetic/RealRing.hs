{-# LANGUAGE RebindableSyntax #-}

module Formulative.Calculation.Algebra.Arithmetic.RealRing where

import Control.Algebra
import qualified Data.Vector.Sized as VS
import Debug.SimpleReflect.Expr
import Formulative.Calculation.Algebra.Arithmetic.Additive
import Formulative.Calculation.Algebra.Arithmetic.AdditiveGroup
import Formulative.Calculation.Algebra.Arithmetic.Multiplicative
import Formulative.Calculation.Internal.Types
import GHC.Generics
import GHC.TypeNats
import Prelude hiding (fromInteger)
import qualified Prelude

-- class (AdditiveGroup a, Multiplicative a) => RealRing a where
--     abs :: Integer -> a
--     default fromInteger :: (Generic a, GRing (Rep a)) => Integer -> a
--     fromInteger = to . gfromInteger

-- class (GMultiplicative f, GAdditiveGroup f) => GRing f where
--     gfromInteger :: Integer -> f a
-- instance GRing a => GRing (M1 i c a) where
--     gfromInteger a = M1 (gfromInteger a)
-- instance (GRing a, GRing b) => GRing (a :*: b) where
--     gfromInteger a = gfromInteger a :*: gfromInteger a
-- instance (Ring a) => GRing (K1 i a) where
--     gfromInteger a = K1 (fromInteger a)

-- -- deriving instance
-- instance (Num a) => Ring (MyNum a) where
--     fromInteger = MyNum . Prelude.fromInteger

-- instance (Ring a, Applicative m) => Ring (MyApplicative m a) where
--     fromInteger = MyApplicative . pure . fromInteger

-- deriving via (MyNum Word) instance Ring Word
-- deriving via (MyNum Int) instance Ring Int
-- deriving via (MyNum Integer) instance Ring Integer
-- deriving via (MyNum Float) instance Ring Float
-- deriving via (MyNum Double) instance Ring Double
-- deriving via (MyNum Expr) instance Ring Expr

-- deriving via (MyApplicative IO a) instance (Ring a) => Ring (IO a)
-- deriving via (MyApplicative Maybe a) instance (Ring a) => Ring (Maybe a)
-- deriving via (MyApplicative (Either a) b) instance (Ring b) => Ring (Either a b)
-- deriving via (MyApplicative (VS.Vector n) a) instance (KnownNat n, Ring a) => Ring (VS.Vector n a)