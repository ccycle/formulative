{-# LANGUAGE DuplicateRecordFields #-}

module Test.Optimization.LBFGS where

import Control.Exception.Safe
import qualified Data.Vector as V
import Formulative.Calculation.Optimization.LBFGS
import Formulative.Calculation.Optimization.LineSearch
import Formulative.Preprocess.DefaultValue
import Test.Tasty

x = 0.1 :: Double
f x = (x - 1) ^ 4
gradf x = 4 * (x - 1) ^ 3

x1 = 0.1 :: Double
f1 = negate . cos
gradf1 = sin

x2 = 0.1 :: Double
f2 x = (x - 1) ^ 2 * (x + 1) ^ 2
gradf2 x = 4 * (-1 + x) * x * (1 + x)

c2 = 0.9
alpha = 1.0
lineSearchParamsTest = defaultValue

-- wolfeConditionTest = wolfeCondition (MkWolfeConditionParameter c2) (MkStepSizeForLineSearch alpha) x (MkDescentDirection p) (MkGradObjectiveFunction gradf)

p = -1.0

-- backtrackingLineSearchTest :: (StepSizeForLineSearch Double)
-- backtrackingLineSearchTest = backtrackingLineSearch lineSearchParamsTest x (MkDescentDirection p) (MkObjectiveFunction f) (MkGradObjectiveFunction gradf)

-- lbfgsMethodTest :: MonadThrow m => m (Double, Residuals ((Double)), Residuals (DescentDirection Double))
-- lbfgsMethodTest = lbfgsMethod defaultLineSearchParameters defaultValue defaultValue (MkObjectiveFunction f) (MkGradObjectiveFunction gradf) x

-- lbfgsMethodTest1 :: MonadThrow m => m (Double, Residuals ((Double)), Residuals (DescentDirection Double))
-- lbfgsMethodTest1 = lbfgsMethod defaultLineSearchParameters defaultValue defaultValue (MkObjectiveFunction f1) (MkGradObjectiveFunction gradf1) x1

lbfgsMethodTest2 :: MonadThrow m => Double -> m Double
lbfgsMethodTest2 = lbfgsMethod lineSearchParamsTest defaultValue defaultValue (MkObjectiveFunction f2) (MkGradObjectiveFunction gradf2)