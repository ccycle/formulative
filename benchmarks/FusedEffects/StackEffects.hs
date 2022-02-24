{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Criterion.Main
import Criterion.Types

-- import qualified Data.Matrix.Static.LinearAlgebra.Types as MS
-- import Data.Singletons
-- import Data.Singletons.TypeLits
-- import qualified Eigen.SparseMatrix as ES

import Control.Carrier.Reader
import Control.Effect.Sum
import OptDEC.Calculation.Algebra.Arithmetic.Class
import OptDEC.Calculation.Matrix.Class
import Path
import Path.IO

-- https://haskell.e-bigmoon.com/stack/bench/index.html
-- https://haskell.e-bigmoon.com/posts/2018/06-25-all-about-strictness

-- matrixSizedTest :: forall n. KnownNat n => MS.SparseMatrix n n Double -> MS.SparseMatrix n n Double
-- matrixSizedTest x = x .*. x

-- eigenTest :: forall n. KnownNat n => ES.SparseMatrix n n Double -> ES.SparseMatrix n n Double
-- eigenTest x = x .*. x

newtype ConfigTest1 = MkConfigTest1 String
newtype ConfigTest2 = MkConfigTest2 String
newtype ConfigTest3 = MkConfigTest3 String
newtype ConfigTest4 = MkConfigTest4 String
newtype ConfigTest5 = MkConfigTest5 String

data EnvTest = MkEnvTest
    { test1 :: ConfigTest1
    , test2 :: ConfigTest2
    , test3 :: ConfigTest3
    , test4 :: ConfigTest4
    , test5 :: ConfigTest5
    }

runTest
    ( MkEnvTest
            (MkConfigTest1 test1)
            (MkConfigTest2 test2)
            (MkConfigTest3 test3)
            (MkConfigTest4 test4)
            (MkConfigTest5 test5)
        ) =
        runReader
            (MkConfigTest1 test1)
            . runReader (MkConfigTest2 test2)
            . runReader (MkConfigTest3 test3)
            . runReader (MkConfigTest4 test4)
            . runReader (MkConfigTest5 test5)
effTest ::
    ( Monad m
    , Algebra sig m
    , Member (Reader ConfigTest1) sig
    , Member (Reader ConfigTest2) sig
    , Member (Reader ConfigTest3) sig
    , Member (Reader ConfigTest4) sig
    , Member (Reader ConfigTest5) sig
    ) =>
    m [Char]
effTest = do
    MkConfigTest1 t1 <- ask
    MkConfigTest2 t2 <- ask
    MkConfigTest3 t3 <- ask
    MkConfigTest4 t4 <- ask
    MkConfigTest5 t5 <- ask
    return $ concat [t1, t2, t3, t4, t5]

-- actionTest
defaultEnvTest = MkEnvTest (MkConfigTest1 "test1") (MkConfigTest2 "test2") (MkConfigTest3 "test3") (MkConfigTest4 "test4") (MkConfigTest5 "test5")
runEffTest1 str = run $ runTest str effTest
effTest2 :: (Monad m, Member (Reader EnvTest) sig, Algebra sig m) => m [Char]
effTest2 = do
    MkEnvTest (MkConfigTest1 t1) (MkConfigTest2 t2) (MkConfigTest3 t3) (MkConfigTest4 t4) (MkConfigTest5 t5) <- ask
    return $ concat [t1, t2, t3, t4, t5]
runEffTest2 str = run $ runReader @EnvTest str effTest2

-- 巨大なEnvを一つもってasks ... で取り出すのか
-- Readerで持っておいて複数のrunReaderを実行するのか

criterionReport path =
    defaultMainWith
        (defaultConfig{reportFile = path})
        [ bgroup
            "nested runReader vs large Data type"
            [ bench "nested runReader" $ whnf runEffTest1 defaultEnvTest
            , bench "large Data type" $ whnf runEffTest2 defaultEnvTest
            ]
        ]

-- , bench "n=100" $ whnf (matrixSizedTest @100) one
-- , bench "n=1000" $ whnf (matrixSizedTest @1000) one
-- , bench "n=10000" $ whnf (matrixSizedTest @10000) one
-- , bench "n=100000" $ whnf (matrixSizedTest @100000) one

-- , bgroup
--     "large Config"
--     [
--         bench "n=10" $ whnf (eigenTest @10) one
--     , bench "n=100" $ whnf (eigenTest @100) one
--     , bench "n=1000" $ whnf (eigenTest @1000) one
--     -- , bench "n=10000" $ whnf (eigenTest @10000) one
--     -- , bench "n=100000" $ whnf (eigenTest @100000) one
--     ]
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

main :: IO ()
main = do
    let relDirName = "./benchmarks/FusedEffects"
    let srcDir = parseRelDir relDirName -- Maybe
    let srcDir' = parseRelDir relDirName -- IO
    let fileName = addExtension ".html" =<< parseRelFile "StackingEffects"
    ensureDir @IO <$> srcDir'
    criterionReport (toFilePath <$> ((</>) <$> srcDir <*> fileName))
