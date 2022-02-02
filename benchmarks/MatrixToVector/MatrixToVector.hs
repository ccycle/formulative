{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Monad.ST.Strict
import Criterion.Main
import Criterion.Types
import qualified Data.Matrix.Static.Dense as MSD
import qualified Data.Matrix.Static.Generic as MSG
import qualified Data.Matrix.Static.LinearAlgebra as MSL
import qualified Data.Matrix.Static.LinearAlgebra.Types as MSL
import qualified Data.Matrix.Static.Sparse as MSS
import Data.Maybe (fromJust)
import Data.STRef.Strict
import Data.Singletons
import Data.Singletons.TypeLits
import qualified Data.Vector as V
import qualified Data.Vector.Sized as VS
import qualified Data.Vector.Unboxed as VU
import qualified Eigen.Internal as E
import qualified Eigen.Matrix as E
import qualified Eigen.SparseMatrix as ES
import OptDEC.Calculation.Algebra.Arithmetic.Class
import OptDEC.Calculation.Matrix.Class
import Path
import Path.IO

-- https://haskell.e-bigmoon.com/stack/bench/index.html
-- https://haskell.e-bigmoon.com/posts/2018/06-25-all-about-strictness

-- ベクトルに変換してからindexを参照するのがいいのかどうかを検証
matrixSizedMulAdjTest :: forall n p. (KnownNat n, KnownNat p) => MSL.SparseMatrix n p Double -> MSL.SparseMatrix n n Double
matrixSizedMulAdjTest x = x .@. (one :: MSL.SparseMatrix p n Double)

eigenMulAdjTest :: forall n p. (KnownNat n, KnownNat p) => ES.SparseMatrix n p Double -> ES.SparseMatrix n n Double
eigenMulAdjTest x = x .@. (one :: ES.SparseMatrix p n Double)

coo = [(0, 0, 1), (1, 1, 1), (2, 2, 1)]
cooMatrixSized :: forall p. (KnownNat p) => MSL.SparseMatrix p 3 Double
cooMatrixSized = MSS.fromTriplet (VU.fromList coo) :: MSL.SparseMatrix p 3 Double
cooEigen :: forall p. (KnownNat p) => ES.SparseMatrix p 3 Double
cooEigen = ES.fromList coo :: ES.SparseMatrix p 3 Double

vectorTest :: forall n p. (KnownNat n, KnownNat p) => VS.Vector p (VS.Vector n Double)
vectorTest = one

-- position :: Vector n (MSL.SparseMatrix p 1 a)
-- 位置ベクトルの列から一部を選択して行列を作成するコストはどれくらいか

convertVectorToMatrixMS :: forall n p. (KnownNat n, KnownNat p) => VS.Vector p (VS.Vector n Double) -> MSL.SparseMatrix p n Double
convertVectorToMatrixMS = MSS.fromVector . V.convert . VS.foldl' (V.++) (V.fromList []) . sequence . VS.fromSized

convertToMatrixE :: forall n p. (KnownNat n, KnownNat p) => VS.Vector p (VS.Vector n Double) -> E.Matrix p n Double
convertToMatrixE = fromJust . E.fromList . VS.toList . VS.map VS.toList

positionsVectorTest :: forall n p. (KnownNat n, KnownNat p) => VS.Vector p (VS.Vector n Double)
positionsVectorTest = one

sizedSimplesTest :: VS.Vector 3 Int
sizedSimplesTest = fromJust . VS.fromList $ [2, 3, 5]

selectPositionTest ::
    forall n p.
    (KnownNat n, KnownNat p) =>
    VS.Vector p (VS.Vector n Double) ->
    E.Matrix 3 n Double
selectPositionTest x = convertToMatrixE $ VS.map (VS.unsafeIndex x) sizedSimplesTest

-- >>> v = V.fromList [0,1,2,3,4,5]
-- >>> MSS.fromVector v :: MSS.SparseMatrix 3 2 V.Vector Double
-- (3 x 2)
-- 0.0 3.0
-- 1.0 4.0
-- 2.0 5.0

-- TODO: reportFileで指定されているファイルパスのディレクトリが存在するかどうかのを判定する関数を作成
matrixToVectorReport path =
    defaultMainWith
        (defaultConfig{reportFile = path})
        [ bgroup
            "Matrix-Sized: multiply adjacency matrix (whnf)"
            [ bench "n=10" $ whnf (matrixSizedMulAdjTest @10) cooMatrixSized
            , bench "n=100" $ whnf (matrixSizedMulAdjTest @100) cooMatrixSized
            , bench "n=1000" $ whnf (matrixSizedMulAdjTest @1000) cooMatrixSized
            , bench "n=10000" $ whnf (matrixSizedMulAdjTest @10000) cooMatrixSized
            ]
        , bgroup
            "Eigen: multiply adjacency matrix (whnf)"
            [ bench "n=10" $ whnf (eigenMulAdjTest @10) cooEigen
            , bench "n=100" $ whnf (eigenMulAdjTest @100) cooEigen
            , bench "n=1000" $ whnf (eigenMulAdjTest @1000) cooEigen
            , bench "n=10000" $ whnf (eigenMulAdjTest @10000) cooEigen
            ]
        , bgroup
            "Eigen: select position (whnf)"
            [ bench "n=10" $ whnf (selectPositionTest @3 @10) one
            , bench "n=100" $ whnf (selectPositionTest @3 @100) one
            , bench "n=1000" $ whnf (selectPositionTest @3 @1000) one
            , bench "n=10000" $ whnf (selectPositionTest @3 @10000) one
            ]
        , bgroup
            "Matrix-Sized: Matrix conversion (whnf)"
            [ bench "n=3, p=10" $ whnf (convertVectorToMatrixMS @3 @10) one
            , bench "n=3, p=100" $ whnf (convertVectorToMatrixMS @3 @100) one
            , bench "n=3, p=1000" $ whnf (convertVectorToMatrixMS @3 @1000) one
            , bench "n=3, p=10000" $ whnf (convertVectorToMatrixMS @3 @10000) one
            ]
        ]

main :: IO ()
main = do
    let relDirName = "./benchmarks/MatrixToVector"
    let srcDir = parseRelDir relDirName -- Maybe
    let srcDir' = parseRelDir relDirName -- IO
    let fileName = addExtension ".html" =<< parseRelFile "MatrixToVector"
    ensureDir @IO <$> srcDir'
    matrixToVectorReport (toFilePath <$> ((</>) <$> srcDir <*> fileName))
