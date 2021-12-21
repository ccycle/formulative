{-# LANGUAGE ScopedTypeVariables #-}

module MatrixMul (main) where

import Criterion.Main
import Criterion.Types
import qualified Data.Matrix.Static.LinearAlgebra.Types as MS
import Data.Singletons
import Data.Singletons.TypeLits
import qualified Eigen.SparseMatrix as ES
import HStructure.Calculation.Algebra.Arithmetic.Class
import HStructure.Calculation.Matrix.Class
import Path
import Path.IO

-- https://haskell.e-bigmoon.com/stack/bench/index.html
-- https://haskell.e-bigmoon.com/posts/2018/06-25-all-about-strictness

matrixSizedTest :: forall n. KnownNat n => MS.SparseMatrix n n Double -> MS.SparseMatrix n n Double
matrixSizedTest x = x .*. x

eigenTest :: forall n. KnownNat n => ES.SparseMatrix n n Double -> ES.SparseMatrix n n Double
eigenTest x = x .*. x

-- factorialList 0 = 1
-- factorialList n
--     | n < 0 = 0
--     | otherwise = product [1 .. n]

-- factorialRecursive 0 = 1
-- factorialRecursive n
--     | n < 0 = 0
--     | otherwise = n * factorialRecursive (n - 1)

-- factorialSTRef n = runST $ do
--     x <- newSTRef 1
--     forM_ [1 .. n] $ \i ->
--         modifySTRef x (* i)
--     readSTRef x

-- numBGroupWHNF func name =
--     bgroup
--         (name ++ " (whnf)")
--         [ bench "n=1" $ whnf func 1
--         , bench "n=10" $ whnf func 10
--         , bench "n=100" $ whnf func 100
--         , bench "n=1000" $ whnf func 1000
--         -- , bench "n=10000" $ whnf (factorial @Int) 10000
--         ]

-- numBGroupNF func name =
--     bgroup
--         (name ++ " (nf)")
--         [ bench "n=1" $ nf func 1
--         , bench "n=10" $ nf func 10
--         , bench "n=100" $ nf func 100
--         , bench "n=1000" $ nf func 1000
--         -- , bench "n=10000" $ whnf (factorial @Int) 10000
--         ]

-- factorialListBenchWHNF = numBGroupWHNF (factorialList :: Integer -> Integer) "factorialList"
-- factorialListBenchNF = numBGroupNF (factorialList :: Integer -> Integer) "factorialList"

-- factorialRecursiveBenchWHNF = numBGroupWHNF (factorialRecursive :: Integer -> Integer) "factorialRecursive"
-- factorialRecursiveBenchNF = numBGroupNF (factorialRecursive :: Integer -> Integer) "factorialRecursive"

-- factorialExactCombinatoricsBenchNF = numBGroupNF (factorial :: Int -> Integer) "factorial from exact-combinatorics"
-- factorialExactCombinatoricsBenchWHNF = numBGroupWHNF (factorial :: Int -> Integer) "factorial from exact-combinatorics"

-- TODO: reportFileで指定されているファイルパスのディレクトリが存在するかどうかのを判定する関数を作成
matMulReport path =
    defaultMainWith
        (defaultConfig{reportFile = path})
        [ bgroup
            "Matrix-Sized (whnf)"
            [ bench "n=10" $ whnf (matrixSizedTest @10) one
            , bench "n=100" $ whnf (matrixSizedTest @100) one
            , bench "n=1000" $ whnf (matrixSizedTest @1000) one
            -- , bench "n=10000" $ whnf (matrixSizedTest @10000) one
            -- , bench "n=100000" $ whnf (matrixSizedTest @100000) one
            ]
        , bgroup
            "Eigen (whnf)"
            [ bench "n=10" $ whnf (eigenTest @10) one
            , bench "n=100" $ whnf (eigenTest @100) one
            , bench "n=1000" $ whnf (eigenTest @1000) one
            -- , bench "n=10000" $ whnf (eigenTest @10000) one
            -- , bench "n=100000" $ whnf (eigenTest @100000) one
            ]
            -- , bgroup
            --     "Matrix-Sized (nf)"
            --     [ bench "n=10" $ nf (matrixSizedTest @10) one
            --     , bench "n=100" $ nf (matrixSizedTest @100) one
            --     , bench "n=1000" $ nf (matrixSizedTest @1000) one
            --     , bench "n=10000" $ nf (matrixSizedTest @10000) one
            --     ]
            -- , bgroup
            --     "Eigen (whnf)"
            --     [ bench "n=10" $ nf (eigenTest @10) one
            --     , bench "n=100" $ nf (eigenTest @100) one
            --     , bench "n=1000" $ nf (eigenTest @1000) one
            --     , bench "n=10000" $ nf (eigenTest @10000) one
            --     ]
        ]

main :: IO ()
main = do
    let srcDir = parseRelDir "./benchmarks"
    let fileName = addExtension ".html" =<< parseRelFile "matrixMul"
    ensureDir @IO <$> parseRelDir "./benchmarks"
    matMulReport (toFilePath <$> ((</>) <$> srcDir <*> fileName))
