{-# LANGUAGE UndecidableInstances #-}

module Formulative.Algebra.Vector.InnerProductSpace where

import Control.Applicative
import Data.Foldable
import qualified Data.Vector.Sized as VS
import Formulative.Algebra.Arithmetic
import Formulative.Algebra.Internal.FromPrelude
import Formulative.Algebra.Prelude
import Formulative.Algebra.Vector.NormSpace
import Formulative.Algebra.Vector.VectorSpace
import GHC.Generics
import GHC.TypeNats
import qualified Prelude as P

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
    (<..>) (x :*: y) (z :*: w) = (<..>) x z + (<..>) y w

instance (P.Num a) => InnerProductSpace (FromPrelude a) where
    (FromPrelude a) <.> (FromPrelude b) = a P.* b

deriving via (FromPrelude Int) instance InnerProductSpace Int
deriving via (FromPrelude Integer) instance InnerProductSpace Integer
deriving via (FromPrelude Double) instance InnerProductSpace Double
deriving via (FromPrelude Float) instance InnerProductSpace Float

instance (VectorSpace a, Multiplicative a, Applicative m, Foldable m) => InnerProductSpace (FromPrelude1 m a) where
    (<.>) (FromPrelude1 x) (FromPrelude1 y) = foldl' (+) zero (liftA2 (*) x y)

deriving via (FromPrelude1 (VS.Vector n) a) instance (Multiplicative a, VectorSpace a, KnownNat n) => InnerProductSpace (VS.Vector n a)

angleV ::
    ( RealField v ~ Scalar v
    , NormSpace v
    , Eq (RealField v)
    , Field (Scalar v)
    , InnerProductSpace v
    , Additive (RealField v)
    ) =>
    RealField v ->
    v ->
    v ->
    Scalar v
angleV lp v1 v2 = if absPowSum lp v1 == zero || absPowSum lp v2 == zero then zero else v1 <.> v2 / (absPowSum lp v1 * absPowSum lp v2)

normalize :: (Eq (Scalar v), InnerProductSpace v, Algebraic (Scalar v)) => v -> v
normalize v = if v <.> v /= zero then v >/ sqrt (v <.> v) else zero