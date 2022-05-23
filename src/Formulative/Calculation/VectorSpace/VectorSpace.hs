{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.VectorSpace.VectorSpace where

import Data.Complex
import qualified Data.Vector.Sized as VS
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Internal.Types
import GHC.Generics
import GHC.Natural
import GHC.TypeNats

class (AdditiveGroup a) => VectorSpace a where
    type Scalar a :: *
    (*.) :: Scalar a -> a -> a
    default (*.) :: (Generic a, GVectorSpace (Rep a), GAdditive (Rep a), GAdditiveGroup (Rep a), Scalar a ~ GScalar (Rep a)) => Scalar a -> a -> a
    type Scalar a = GScalar (Rep a)
    (*.) a b = to (a *.. from b)

    infixr 7 *.

class GVectorSpace f where
    type GScalar f
    (*..) :: GScalar f -> f a -> f a
instance GVectorSpace f => GVectorSpace (M1 i c f) where
    type GScalar (M1 i c f) = GScalar f
    a *.. M1 b = M1 (a *.. b)
instance VectorSpace a => GVectorSpace (K1 i a) where
    type GScalar (K1 i a) = Scalar a
    a *.. K1 b = K1 (a *. b)
instance (GVectorSpace f, GVectorSpace g, GScalar f ~ GScalar g) => GVectorSpace (f :*: g) where
    type GScalar (f :*: g) = GScalar f
    μ *.. (x :*: y) = μ *.. x :*: μ *.. y

-- deriving instance
instance (Num a) => VectorSpace (MyNum a) where
    type Scalar (MyNum a) = a
    a *. (MyNum b) = MyNum (a * b)

deriving via (MyNum Int) instance VectorSpace Int
deriving via (MyNum Integer) instance VectorSpace Integer
deriving via (MyNum Float) instance VectorSpace Float
deriving via (MyNum Double) instance VectorSpace Double
deriving via (MyNum (Complex Float)) instance VectorSpace (Complex Float)
deriving via (MyNum (Complex Double)) instance VectorSpace (Complex Double)
deriving via (MyNum Natural) instance VectorSpace Natural

deriving via (MyNum a) instance (Num a) => VectorSpace (MyFloating a)

-- instance (RealFloat a) => VectorSpace (MyComplex a) where
--     type Scalar (MyComplex a) = Complex a
--     a *. (MyComplex b) = MyComplex (a * b)

----------------------------------------------------------------
------ applicative
----------------------------------------------------------------
instance {-# OVERLAPS #-} (VectorSpace a, Multiplicative a, Applicative m) => VectorSpace (MyApplicative m a) where
    type Scalar (MyApplicative m a) = a
    (*.) x (MyApplicative y) = MyApplicative (fmap (x .*.) y)

deriving via (MyApplicative (VS.Vector n) a) instance (VectorSpace a, Multiplicative a, KnownNat n) => VectorSpace (VS.Vector n a)

instance (VectorSpace a, Applicative m, Foldable m) => VectorSpace (MyFoldable m a) where
    type Scalar (MyFoldable m a) = Scalar a
    (*.) x (MyFoldable y) = MyFoldable (fmap (x *.) y)

x ./ a = reciprocal a *. x
infixl 7 ./