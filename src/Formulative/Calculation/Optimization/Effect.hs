{-# LANGUAGE AllowAmbiguousTypes #-}

module Formulative.Calculation.Optimization.Effect where

import Control.Algebra
import Data.Kind
import Formulative.Calculation.Optimization.LBFGS
import Formulative.Calculation.Optimization.LineSearch
import Formulative.Calculation.Optimization.Setting

data Optimization a (m :: Type -> Type) k where
    AskOptimizationParameters :: Optimization a m (OptimizationSetting a)

askOptimizationParameters :: (Has (Optimization a) sig m) => m (OptimizationSetting a)
askOptimizationParameters = send AskOptimizationParameters

askLBFGSParameters :: forall a sig m. (Has (Optimization a) sig m) => m LBFGSParameters
askLBFGSParameters = lbfgs <$> askOptimizationParameters @a
askLineSearchParameters :: (Has (Optimization a) sig m) => m (LineSearchParameters a)
askLineSearchParameters = lineSearch <$> askOptimizationParameters
askConvergenceTestParameters :: (Has (Optimization a) sig m) => m (ConvergenceTestParameters a)
askConvergenceTestParameters = convergenceTest <$> askOptimizationParameters