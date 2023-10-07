{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Algebra.Vector.VectorSpace where

import Control.Monad.ST.Strict
import Data.Complex
import Data.Foldable (forM_)
import Data.STRef.Strict
import qualified Data.Vector.Sized as VS
import Formulative.Algebra.Arithmetic
import Formulative.Algebra.Internal.FromPrelude
import Formulative.Algebra.Prelude
import GHC.Generics
import GHC.TypeNats
import qualified Prelude as P

class (AdditiveGroup a) => VectorSpace a where
    type Scalar a
    (*<) :: Scalar a -> a -> a
    default (*<) :: (Generic a, GVectorSpace (Rep a), Scalar a ~ GScalar (Rep a)) => Scalar a -> a -> a
    type Scalar a = GScalar (Rep a)
    (*<) a b = to (a *.. from b)

    infixr 7 *<

class GVectorSpace f where
    type GScalar f
    (*..) :: GScalar f -> f a -> f a
instance GVectorSpace f => GVectorSpace (M1 i c f) where
    type GScalar (M1 i c f) = GScalar f
    a *.. M1 b = M1 (a *.. b)
instance VectorSpace a => GVectorSpace (K1 i a) where
    type GScalar (K1 i a) = Scalar a
    a *.. K1 b = K1 (a *< b)
instance (GVectorSpace f, GVectorSpace g, GScalar f ~ GScalar g) => GVectorSpace (f :*: g) where
    type GScalar (f :*: g) = GScalar f
    μ *.. (x :*: y) = μ *.. x :*: μ *.. y

-- deriving instance
instance (P.Num a) => VectorSpace (FromPrelude a) where
    type Scalar (FromPrelude a) = a
    a *< (FromPrelude b) = FromPrelude (a P.* b)

deriving via (FromPrelude Int) instance VectorSpace Int
deriving via (FromPrelude Integer) instance VectorSpace Integer
deriving via (FromPrelude Float) instance VectorSpace Float
deriving via (FromPrelude Double) instance VectorSpace Double
deriving via (FromPrelude (Complex Float)) instance VectorSpace (Complex Float)
deriving via (FromPrelude (Complex Double)) instance VectorSpace (Complex Double)

----------------------------------------------------------------
------ applicative
----------------------------------------------------------------
instance (VectorSpace a, Multiplicative a, Applicative m) => VectorSpace (FromPrelude1 m a) where
    type Scalar (FromPrelude1 m a) = a
    (*<) x (FromPrelude1 y) = FromPrelude1 (fmap (x *) y)

instance (VectorSpace b, AdditiveGroup a) => VectorSpace (a -> b) where
    type Scalar (a -> b) = Scalar b
    x *< f = \y -> x *< f y

deriving via (FromPrelude1 (VS.Vector n) a) instance (VectorSpace a, Multiplicative a, KnownNat n) => VectorSpace (VS.Vector n a)

(>/) :: (VectorSpace a, Field (Scalar a)) => a -> Scalar a -> a
x >/ a = recip a *< x
infixl 7 >/

symmetrizePoly :: (VectorSpace a, Field (Scalar a), Multiplicative a) => Natural -> a -> a -> a
symmetrizePoly n a b = runST $ do
    x0 <- newSTRef zero
    forM_ ns $ \(i, j) -> do
        modifySTRef x0 (+ (a ^+ i * b ^+ j))
    modifySTRef x0 (>/ (fromInteger . naturalToInteger) (n + 1))
    readSTRef x0
  where
    ns = [(i, j) | i <- [0 .. n :: Natural], j <- [0 .. n], i P.+ j == n]