module Test.Optimization.LBFGS where

import Control.Exception.Safe
import qualified Data.Vector as V
import HStructure.Calculation.Optimization.LBFGS
import HStructure.Calculation.Optimization.LineSearch
import Test.Tasty

x = 0.1 :: Double
f x = (x - 1) ^ 4
gradf x = 4 * (x - 1) ^ 3

x1 = 0.1 :: Double
f1 = negate . cos
gradf1 = sin

x2 = 0.1 :: Double
f2 x = ((x - 1) ^ 2) * ((x + 1) ^ 2)
gradf2 x = 4 * (-1 + x) * x * (1 + x)

c2 = 0.9
alpha = 1.0
lineSearchParamsTest = defaultLineSearchParameters{lineSearchConditionType = Armijo, armijoConditionParameter = MkArmijoConditionParameter 1e-4, factorForBacktrackingLineSearch = MkBacktrackingFactor (0.5)}
wolfeConditionTest = wolfeCondition (MkWolfeConditionParameter c2) (MkStepSizeForLineSearch alpha) x (MkDescentDirection p) (MkGradObjectiveFunction gradf)

p = -1.0
backtrackingLineSearchTest :: (StepSizeForLineSearch Double)
backtrackingLineSearchTest = getStepSizeForLineSearch lineSearchParamsTest x (MkDescentDirection p) (MkObjectiveFunction f) (MkGradObjectiveFunction gradf)

-- lbfgsMethodTest :: MonadThrow m => m (Double, Residuals ((Double)), Residuals (DescentDirection Double))
-- lbfgsMethodTest = lbfgsMethod defaultLineSearchParameters defaultConvergenceTestParameters defaultLBFGSParameters (MkObjectiveFunction f) (MkGradObjectiveFunction gradf) x

-- lbfgsMethodTest1 :: MonadThrow m => m (Double, Residuals ((Double)), Residuals (DescentDirection Double))
-- lbfgsMethodTest1 = lbfgsMethod defaultLineSearchParameters defaultConvergenceTestParameters defaultLBFGSParameters (MkObjectiveFunction f1) (MkGradObjectiveFunction gradf1) x1

lbfgsMethodTest2 :: MonadThrow m => Double -> m (Double, Residuals (StepSizeForLineSearch (Double)), Residuals (DescentDirection Double))
lbfgsMethodTest2 x = lbfgsMethod lineSearchParamsTest defaultConvergenceTestParameters defaultLBFGSParameters (MkObjectiveFunction f2) (MkGradObjectiveFunction gradf2) x