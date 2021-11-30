module Test.RefineException where

import Control.Exception.Safe
import qualified Data.Vector.Sized as VS
import Refined
import Test.Tasty

import Control.Applicative (liftA2)
import HStructure.Calculation.Algebra.Arithmetic.Class
import HStructure.Preprocess.Exception

allNonzeroTestVec :: MonadThrow m => m (VS.Vector 3 Double)
allNonzeroTestVec = maybeToMonadThrow FromListException (VS.fromList [2, 1, 1])

allNonzeroRefinedTest :: (MonadCatch m) => m (Refined Dividable (VS.Vector 3 Double))
allNonzeroRefinedTest = do
    a <- allNonzeroTestVec
    refineThrow @Dividable a

notAllNonzeroTestVec :: MonadThrow m => m (VS.Vector 3 Double)
notAllNonzeroTestVec = maybeToMonadThrow FromListException (VS.fromList [1, 1, 0])

notAllNonzeroRefinedTest :: (MonadCatch m) => m (Refined Dividable (VS.Vector 3 Double))
notAllNonzeroRefinedTest = do
    a <- notAllNonzeroTestVec
    refineThrow @Dividable a

unit_AllNonZero = do
    a <- allNonzeroRefinedTest
    print a

-- should be failed
unit_notAllZero = do
    a <- notAllNonzeroRefinedTest
    print a

unit_DivideByVector = do
    a <- allNonzeroRefinedTest
    b <- notAllNonzeroTestVec
    print $ b `safeDiv` a

-- should be failed
unit_DivideByVectorFail = do
    a <- notAllNonzeroRefinedTest
    b <- allNonzeroTestVec
    print $ b `safeDiv` a
