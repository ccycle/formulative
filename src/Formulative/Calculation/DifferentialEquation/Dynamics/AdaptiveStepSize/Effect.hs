module Formulative.Calculation.DifferentialEquation.Dynamics.AdaptiveStepSize.Effect where

import Control.Algebra
import Control.Effect.Sum
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.DifferentialEquation.Dynamics.Class
import Formulative.Calculation.DifferentialEquation.Dynamics.Effect
import Formulative.Calculation.DifferentialEquation.Types
import Formulative.Calculation.VectorSpace.Class

data AdaptiveStepSize a m k where
    AskAdaptiveStepSizeSetting :: AdaptiveStepSize a m (AdaptiveStepSizeSetting a)

askAdaptiveStepSizeSetting :: (Has (AdaptiveStepSize a) sig m) => m (AdaptiveStepSizeSetting a)
askAdaptiveStepSizeSetting = send AskAdaptiveStepSizeSetting

-- Reference:
-- E. N. Lages, E. S. S. Silveira, D. T. Cintra, and A. C. Frery, “An adaptive time integration strategy based on displacement history curvature,” Int. J. Numer. Meth. Engng, vol. 93, no. 12, pp. 1235–1254, Mar. 2013, doi: 10.1002/nme.4421.
getAdaptiveStepSize (StepSize maxStepSize') (AdaptiveStepSizeSetting (StepSize minStepSize') b') f1 f2 x =
    let k = curvatureValueSpaceTime f1 f2 x in StepSize $ max minStepSize' (maxStepSize' .*. exp (negation b' .*. k))
getAdaptiveStepSizeM x = do
    k <- curvatureValueSpaceTimeM x
    (StepSize maxStepSize') <- askStepSize
    AdaptiveStepSizeSetting (StepSize minStepSize') b' <- askAdaptiveStepSizeSetting
    return $ StepSize $ max minStepSize' (maxStepSize' .*. exp (negation b' .*. k))
