{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Test.RefineException where

import Control.Exception.Safe
import qualified Data.Vector.Sized as VS
import Refined
import Test.Tasty

import Control.Applicative (liftA2)
import HStructure.Calculation.Operator.Arithmetic.Class
import HStructure.Preprocess.Exception

allNonzeroTestVec :: MonadThrow m => m (VS.Vector 3 Double)
allNonzeroTestVec = maybeToMonadThrow FromListException (VS.fromList [2, 1, 1])

notAllNonzeroTestVec :: MonadThrow m => m (VS.Vector 3 Double)
notAllNonzeroTestVec = maybeToMonadThrow FromListException (VS.fromList [1, 1, 0])

-- allNonzeroTest :: (MonadCatch m) => m (VS.Vector 3 (Refined NonAdditiveZero Double))
allNonzeroTest :: (MonadCatch m) => m (Refined NonAdditiveZero (VS.Vector 3 Double))
allNonzeroTest = do
    a <- allNonzeroTestVec
    refineThrow @NonAdditiveZero a

-- mapM (refineThrow @NonAdditiveZero) a -- Applocative の中身にrefineThrowを適用したいときにはmapMを使う

notAllNonzeroTest :: (MonadCatch m) => m (VS.Vector 3 (Refined NonAdditiveZero Double))
notAllNonzeroTest = do
    a <- notAllNonzeroTestVec
    mapM (refineThrow @NonAdditiveZero) a

unit_AllZero = do
    a <- allNonzeroTest
    print a

unit_DivideVector = do
    a <- allNonzeroTest
    b <- notAllNonzeroTestVec
    print $ b ./. a

-- should be failed
unit_notAllZero = do
    a <- notAllNonzeroTest
    print a