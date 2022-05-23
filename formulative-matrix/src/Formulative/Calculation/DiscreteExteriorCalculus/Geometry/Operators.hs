{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC-fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC-fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}

module Formulative.Calculation.DiscreteExteriorCalculus.Geometry.Operators where

import qualified Data.Matrix.Static.LinearAlgebra as MSL
import qualified Data.Matrix.Static.Sparse as MSS
import Data.Proxy
import Data.Type.Equality
import qualified Data.Vector as V
import qualified Data.Vector.Sized as VS
import Data.Vector.Storable (Storable)

-- import qualified Eigen.Matrix as E
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.DiscreteExteriorCalculus.Geometry.Types
import Formulative.Calculation.DiscreteExteriorCalculus.Homology.Operators
import Formulative.Calculation.DiscreteExteriorCalculus.Homology.Types
import Formulative.Calculation.Internal.List
import Formulative.Calculation.Internal.TypeLevelNatural
import Formulative.Calculation.Matrix.Class
import Formulative.Calculation.Matrix.Types
import Formulative.Calculation.VectorSpace.Class
import GHC.Exts (IsList (Item, fromList, toList))
import GHC.TypeNats

-- TODO: Euclidean dimとBase dimを型レベルで区別するように変更
simplexToPositionMat ::
  forall nEuc p k a.
  ( HMatrixElement a
  , MSS.Zero a
  , KnownNat nEuc
  , KnownNat k
  , KnownNat p
  ) =>
  AllPointDataPrimal0 nEuc p a ->
  Simplex k ->
  PositionMatrix nEuc k a
simplexToPositionMat (AllPointDataPrimal0 vec) (Simplex s) =
  PositionMatrix mat
 where
  f v i = mapG (`unsafeIndex` i) v
  vec' = mapG (f vec) s
  mat = convertToMatrixE vec'

-- primal volume: ( 1 / k! ) * sqrt ( det ( (V^T) * V ) )
-- Reference: N. Bell and A. N. Hirani, “PyDEC: Software and Algorithms for Discretization of Exterior Calculus,” Mar. 2011. https://arxiv.org/abs/1103.3076v2.

-- [x0,x1,x2,...]->[x1-x0,x2-x0,...]
-- [ [-1, -1]
-- , [ 1,  0]
-- , [ 0,  1]
-- ] .@. [x0,x1,x2]
-- = [x1-x0,x2-x0]
positionsMatRelative :: forall n k a. (KnownNat n, KnownNat k, 1 <= k, Additive a, HMatrixElement a) => PositionMatrix n k a -> PositionMatrix n (k - 1) a
positionsMatRelative (PositionMatrix mat) = PositionMatrix $ mat' .@. mat
 where
  f (i, j) x
    | j == 0 = -1
    | i + 1 == j = 1
    | otherwise = x
  mat' = imap f (zero :: GMatrixContainer k (k + 1) a)

primalVolumeInternal' ::
  forall nEuc l k a p.
  ( HMatrixElement a
  , Floating a
  , MSS.Zero a
  , Additive a
  , KnownNat k
  , KnownNat nEuc
  , (1 <=? k) ~ 'True
  , KnownNat p
  ) =>
  AllPointDataPrimal0 nEuc p a ->
  Simplex k ->
  a
primalVolumeInternal' mat s = recip (factorialNum kInt) * (sqrt . det $ mat' .@. transpose mat')
 where
  mat' = unPositionMatrix $ positionsMatRelative $ simplexToPositionMat mat s
  kInt = natToInt (Proxy :: Proxy k)

primalVolume0Internal ::
  ( HMatrixElement a
  , MSS.Zero a
  , Additive a
  , Multiplicative a
  ) =>
  AllPointDataPrimal0 nEuc l a ->
  Simplex 0 ->
  a
primalVolume0Internal _ _ = one

primalVolumeInternal ::
  forall nEuc p k a.
  ( HMatrixElement a
  , MSS.Zero a
  , Floating a
  , Additive a
  , KnownNat k
  , KnownNat nEuc
  , Multiplicative a
  , KnownNat p
  ) =>
  AllPointDataPrimal0 nEuc p a ->
  Simplex k ->
  a
primalVolumeInternal = case sameNat (Proxy :: Proxy 0) (Proxy :: Proxy k) of
  Just Refl -> primalVolume0Internal
  Nothing ->
    case leqNat (Proxy :: Proxy 1) (Proxy :: Proxy k) of
      Just Refl -> primalVolumeInternal'
      Nothing -> error "primalVolumeInternal"

-- TODO: 自然数kに対し k<1 <=> k=0 の証明を書く

-- >>> vecListsTest = fromJust . VS.fromList @2  $ Prelude.map (fromJust . VS.fromList @4 @Double) ([[0,1,2,3],[4,5,6,7]])
-- >>> vecListsTest
-- Vector [Vector [0.0,1.0,2.0,3.0],Vector [4.0,5.0,6.0,7.0]]
convertVectorToMatrixMS :: forall n p a. (Storable a, KnownNat n, KnownNat p, MSS.Zero a) => VS.Vector n (VS.Vector p a) -> MSL.SparseMatrix p n a
convertVectorToMatrixMS = MSS.fromVector . V.convert . VS.foldl' (<>) (fromList []) . mapG VS.fromSized

-- >>> E.fromList [[0,1,2],[3,4,5]] :: Maybe (E.Matrix 2 3 Double)
-- Just Matrix 2x3
-- 0.0     1.0     2.0
-- 3.0     4.0     5.0
convertToMatrixE ::
  forall n p a t1 t2.
  ( HMatrixElement a
  , IsList t1
  , MapClass t2 t1
  , Item t1 ~ a
  , IsList (t2 t1)
  , IsList (t2 t1)
  , Item (t2 [a]) ~ [a]
  , IsList (t2 [a])
  , Item (t2 t1) ~ t1
  , KnownNat n
  , KnownNat p
  ) =>
  t2 t1 ->
  HMatrixSized n p a
convertToMatrixE = fromList . toList . mapG toList

-- add zero
-- [v0,v1,v2] -> [v0,v1,v2,0]
addZeroPosMat :: forall n k a. (KnownNat k, HMatrixElement a, MSS.Zero a, KnownNat n) => PositionMatrix n k a -> PositionMatrix n (k + 1) a
addZeroPosMat (PositionMatrix sMat) = PositionMatrix $ vConcat sMat (zero :: HMatrixSized 1 n a)

-- generate the following matrix from vectors:
-- Ax = b,
-- A=
-- [2*(v0,v0) 2*(v1,v0) ... 2*(vp,v0) 1
--  2*(v0,v1) 2*(v1,v1) ... 2*(vp,v1) 1
--     .     .
--     .         .
--  2*(v0,vp)    ...        2*(vp,vp) 1
--        1       1 ...             1 0]
-- , b = [ ||v0||^2
--         ||v1||^2
--         .
--         .
--         ||vp||^2
--         1
--        ]
-- where vi (i=0,1,...,p) is a position vector
-- References:
-- Bell, Hirani 2011 https://arxiv.org/abs/1103.3076v2
-- http://mtao.graphics/2017-11-03-simpliical-circumcenters.html
circumcenterAMat :: forall n k a. (HMatrixElement a, MSS.Zero a, KnownNat n, KnownNat k) => PositionMatrix n k a -> GMatrixContainer (k + 2) (k + 2) a
circumcenterAMat (PositionMatrix sMat) = sMat'''
 where
  sMat' = unPositionMatrix $ addZeroPosMat (PositionMatrix sMat)
  sMat'' = sMat' .@. transpose sMat'
  sMat''' = imap f sMat''
  kInt = natToInt (Proxy :: Proxy k)
  f (i, j) x
    | (i == kInt + 1) /= (j == kInt + 1) = 1
    | i == kInt + 1 && j == kInt + 1 = 0
    | otherwise = 2 * x

-- TODO: 行列積とベクトルへの変換コストを比較(ベクトルに変換、1を末尾に足す、行列に戻すという操作だと計算速度は遅いのかを調査)
-- b = [ ||v0||^2
--       ||v1||^2
--       .
--       .
--       ||vp||^2
--       1
--      ]
circumcenterbVec :: forall n k a. (Field a, HMatrixElement a, MSS.Zero a, KnownNat n, KnownNat k) => PositionMatrix n k a -> GMatrixContainer (k + 2) 1 a
circumcenterbVec (PositionMatrix mat) = (mat .^ 2 .@. (one :: GMatrixContainer n 1 a)) `vConcat` one

circumcenterInternal sMat = solveCG a b
 where
  a = circumcenterAMat sMat
  b = circumcenterbVec sMat

circumcenterInternalUnsafe ::
  forall n k a.
  ( Field a
  , HMatrixElement a
  , MSS.Zero a
  , RealFloat a
  , KnownNat k
  , KnownNat n
  ) =>
  PositionMatrix n k a ->
  (BarycentricCoordinate (k + 1) a, Circumcenter n a, Circumradius a)
circumcenterInternalUnsafe sMat = (BarycentricCoordinate x', Circumcenter bx, Circumradius r)
 where
  a = circumcenterAMat sMat
  b = circumcenterbVec sMat
  b' = dropLastRow b
  x = solveCGunsafe a b
  x' = dropLastRow x
  kInt = natToInt (Proxy :: Proxy k)
  q = unsafeIndexMat x (kInt + 1, 0)
  bx = transpose x' .@. unPositionMatrix sMat
  r = sqrt $ q .+. bx <.> bx

-- dualVolume :: forall n l k. Simplices (n-k) l ->
-- dualVolume = undefined