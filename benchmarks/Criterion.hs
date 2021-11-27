{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Criterion.Main
import Criterion.Types

import Math.Combinatorics.Exact.Factorial (factorial)

-- import HStructure.Calculation.DEC.Generator

-- https://haskell.e-bigmoon.com/stack/bench/index.html
-- https://haskell.e-bigmoon.com/posts/2018/06-25-all-about-strictness

factorialList 0 = 1
factorialList n
    | n < 0 = 0
    | otherwise = product [1 .. n]

factorialRecursive 0 = 1
factorialRecursive n
    | n < 0 = 0
    | otherwise = n * factorialRecursive (n - 1)

numBGroupWHNF func name =
    bgroup
        (name ++ " (whnf)")
        [ bench "n=1" $ whnf func 1
        , bench "n=10" $ whnf func 10
        , bench "n=100" $ whnf func 100
        , bench "n=1000" $ whnf func 1000
        -- , bench "n=10000" $ whnf (factorial @Int) 10000
        ]

numBGroupNF func name =
    bgroup
        (name ++ " (nf)")
        [ bench "n=1" $ nf func 1
        , bench "n=10" $ nf func 10
        , bench "n=100" $ nf func 100
        , bench "n=1000" $ nf func 1000
        -- , bench "n=10000" $ whnf (factorial @Int) 10000
        ]

factorialListBenchWHNF = numBGroupWHNF (factorialList :: Integer -> Integer) "factorialList"
factorialListBenchNF = numBGroupNF (factorialList :: Integer -> Integer) "factorialList"

factorialRecursiveBenchWHNF = numBGroupWHNF (factorialRecursive :: Integer -> Integer) "factorialRecursive"
factorialRecursiveBenchNF = numBGroupNF (factorialRecursive :: Integer -> Integer) "factorialRecursive"

factorialExactCombinatoricsBenchNF = numBGroupNF (factorial :: Int -> Integer) "factorial from exact-combinatorics"
factorialExactCombinatoricsBenchWHNF = numBGroupWHNF (factorial :: Int -> Integer) "factorial from exact-combinatorics"

factorialReport =
    defaultMainWith
        (defaultConfig{reportFile = Just "./benchmark-criterion/factorial-report.html"})
        [ factorialListBenchWHNF
        , factorialListBenchNF
        , factorialRecursiveBenchWHNF
        , factorialRecursiveBenchNF
        , factorialExactCombinatoricsBenchWHNF
        , factorialExactCombinatoricsBenchNF
        ]

main :: IO ()
main = factorialReport
