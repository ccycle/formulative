{-# LANGUAGE AllowAmbiguousTypes #-}

module Formulative.Calculation.Optimization.Effect where

import Control.Algebra
import Data.Kind
import Formulative.Calculation.Optimization.LBFGS
import Formulative.Calculation.Optimization.LineSearch
import Formulative.Calculation.Optimization.Parameter

data Optimization a (m :: Type -> Type) k where
    AskOptimizationParameters :: Optimization a m (OptimizationParameters a)

askOptimizationParameters :: (Has (Optimization a) sig m) => m (OptimizationParameters a)
askOptimizationParameters = send AskOptimizationParameters

askLBFGSParameters :: forall a sig m. (Has (Optimization a) sig m) => m LBFGSParameters
askLBFGSParameters = lbfgsParameters <$> askOptimizationParameters @a
askLineSearchParameters :: (Has (Optimization a) sig m) => m (LineSearchParameters a)
askLineSearchParameters = lineSearchParameters <$> askOptimizationParameters
askConvergenceTestParameters :: (Has (Optimization a) sig m) => m (ConvergenceTestParameters a)
askConvergenceTestParameters = convergenceTest <$> askOptimizationParameters