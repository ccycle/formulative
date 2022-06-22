{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.VectorSpace.InnerProductSpace where

import Control.Algebra
import Control.Applicative
import Data.Foldable
import qualified Data.Vector.Sized as VS
import Formulative.Calculation.Algebra.Arithmetic
import Formulative.Calculation.Internal.Types
import Formulative.Calculation.VectorSpace.NormSpace
import Formulative.Calculation.VectorSpace.VectorSpace
import GHC.Generics
import GHC.Natural
import GHC.TypeNats

class (VectorSpace v) => InnerProductSpace v where
    (<.>) :: v -> v -> Scalar v
    default (<.>) :: (Generic v, GInnerProductSpace (Rep v), GScalar (Rep v) ~ Scalar v) => v -> v -> Scalar v
    (<.>) a b = (<..>) (from a) (from b)

    infixr 8 <.>

class GInnerProductSpace f where
    (<..>) :: f v -> f v -> GScalar f
instance InnerProductSpace s => GInnerProductSpace (K1 i s) where
    (<..>) (K1 v) (K1 w) = (<.>) v w
instance (GInnerProductSpace a) => GInnerProductSpace (M1 i c a) where
    (<..>) (M1 v) (M1 w) = (<..>) v w
instance (GInnerProductSpace f, GInnerProductSpace g, GScalar g ~ GScalar f, Additive (GScalar f)) => GInnerProductSpace (f :*: g) where
    (<..>) (x :*: y) (z :*: w) = (<..>) x z .+. (<..>) y w

instance (Num a) => InnerProductSpace (MyNum a) where
    (MyNum a) <.> (MyNum b) = a * b

deriving via (MyNum Int) instance InnerProductSpace Int
deriving via (MyNum Integer) instance InnerProductSpace Integer
deriving via (MyNum Natural) instance InnerProductSpace Natural
deriving via (MyNum Double) instance InnerProductSpace Double
deriving via (MyNum Float) instance InnerProductSpace Float

instance {-# OVERLAPS #-} (VectorSpace a, Multiplicative a, Applicative m, Foldable m) => InnerProductSpace (MyApplicative m a) where
    (<.>) (MyApplicative x) (MyApplicative y) = foldl' (.+.) zero (liftA2 (.*.) x y)

deriving via (MyApplicative (VS.Vector n) a) instance (Multiplicative a, VectorSpace a, KnownNat n) => InnerProductSpace (VS.Vector n a)

angleV lp v1 v2 = if absPowSum lp v1 == zero || absPowSum lp v2 == zero then zero else v1 <.> v2 ./. (absPowSum lp v1 .*. absPowSum lp v2)

normalize v = if v <.> v /= zero then v ./ sqrt' (v <.> v) else zero