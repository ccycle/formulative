module Formulative.Calculation.Optimization.Class where

import Control.Algebra
import Formulative.Calculation.Algebra.Arithmetic.AdditiveGroup
import Formulative.Calculation.Internal.Variable.Effect
import Formulative.Calculation.VectorSpace.Class

class (VectorSpace a) => HasObjectiveFunctionM m a where
    getObjectiveFunctionM :: m (a -> Scalar a)
    default getObjectiveFunctionM :: (Has (Variable a) sig m, InnerProductSpace a, HasGradObjectiveFunctionM m a) => m (a -> Scalar a)
    getObjectiveFunctionM = do
        xOld <- getVariableOld @a
        gradf <- getGradientOfObjectiveFunctionM
        return $ \x -> (x .-. xOld) <.> gradf x

class (VectorSpace a) => HasGradObjectiveFunctionM m a where
    getGradientOfObjectiveFunctionM :: m (a -> a)
