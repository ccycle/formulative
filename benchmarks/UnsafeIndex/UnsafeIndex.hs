{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Criterion.Main
import Criterion.Types
import qualified Data.Matrix.Static.LinearAlgebra.Types as MSL
import qualified Data.Matrix.Static.Sparse as MSS
import HStructure.Calculation.Algebra.Arithmetic.Class
import HStructure.Calculation.Matrix.Class
import Path
import Path.IO

-- unsafeIndexのパフォーマンスを見る

matMulReport path =
    defaultMainWith
        (defaultConfig{reportFile = path})
        [ bgroup
            "Matrix-Sized: multiply adjacency matrix (whnf)"
            [ bench "n=10" $ whnf (`MSS.unsafeIndex` (10, 1)) (one :: MSL.SparseMatrix 100000 1 Double)
            , bench "n=100" $ whnf (`MSS.unsafeIndex` (100, 1)) (one :: MSL.SparseMatrix 100000 1 Double)
            , bench "n=1000" $ whnf (`MSS.unsafeIndex` (1000, 1)) (one :: MSL.SparseMatrix 100000 1 Double)
            , bench "n=10000" $ whnf (`MSS.unsafeIndex` (10000, 1)) (one :: MSL.SparseMatrix 100000 1 Double)
            ]
        ]

main :: IO ()
main = do
    let relDirName = "./benchmarks/UnsafeIndex"
    let srcDir = parseRelDir relDirName -- Maybe
    let srcDir' = parseRelDir relDirName -- IO
    let fileName = addExtension ".html" =<< parseRelFile "unsafeIndex"
    ensureDir @IO <$> srcDir'
    matMulReport (toFilePath <$> ((</>) <$> srcDir <*> fileName))