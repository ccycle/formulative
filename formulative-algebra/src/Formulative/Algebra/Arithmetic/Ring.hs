{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Algebra.Arithmetic.Ring where

import qualified Data.Vector.Sized as VS
import Debug.SimpleReflect.Expr
import Formulative.Algebra.Arithmetic.AdditiveGroup
import Formulative.Algebra.Arithmetic.Semiring (Semiring)
import Formulative.Algebra.Internal.FromPrelude
import GHC.TypeNats
import Prelude hiding (fromInteger)

class (AdditiveGroup a, Semiring a) => Ring a

instance (Num a) => Ring (FromPrelude a)

deriving via (FromPrelude Int) instance Ring Int
deriving via (FromPrelude Integer) instance Ring Integer
deriving via (FromPrelude Rational) instance Ring Rational
deriving via (FromPrelude Float) instance Ring Float
deriving via (FromPrelude Double) instance Ring Double
deriving via (FromPrelude Expr) instance Ring Expr

instance (Ring a, Applicative m) => Ring (FromPrelude1 m a)
deriving via (FromPrelude1 (VS.Vector n) a) instance (KnownNat n, Ring a) => Ring (VS.Vector n a)