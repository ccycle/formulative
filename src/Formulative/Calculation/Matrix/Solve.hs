-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE InstanceSigs #-}

module Formulative.Calculation.Matrix.Solve where

import Data.Complex
import Data.Singletons.Prelude
import Formulative.Calculation.Matrix.Class
import Formulative.Calculation.VectorSpace.NormSpace
import GHC.TypeNats

class Rank (mat :: MatrixKind) where
    rank :: forall n m a. (KnownNat n, KnownNat m) => mat n m a -> Int

class Eigenvalue (mat :: MatrixKind) where
    eigenvectors :: forall n a. (KnownNat n) => mat n n a -> (mat n 1 (Complex (RealField a)), mat n n (Complex (RealField a)))

class SVD (mat :: MatrixKind) where
    svd :: forall n p a m. (KnownNat n, KnownNat p, KnownNat m, m ~ Min n p) => mat n p a -> (mat n m a, mat m 1 (RealField a), mat p m a)

-- rank
-- eigenvectors
-- eigenvalues
-- ker, im