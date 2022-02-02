{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.DynamicSystem where

import Control.Algebra
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Effect.Sum
import GHC.Generics
import GHC.Natural
import HStructure.Calculation.Algebra.Arithmetic.Class
import HStructure.Calculation.DifferentialEquation.Parameter
import HStructure.Calculation.Internal.Class
import HStructure.Calculation.Optimization.LineSearch
import HStructure.Calculation.VectorSpace.Class
import Test.Tasty

data StateTest = MkTestData {position :: Double, momentum :: Double}
  deriving (Show, Generic)
  deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace)

data GlobalQuantity = MkGlobalQuantity {energy :: Double, internalEnergy :: Double}

data EquationParameter = MkEquationParameter {k :: Double, gamma :: Double}

deltaL
  (MkStepSize dt)
  (MkEquationParameter k gamma)
  (MkTestData x v)
  (MkTestData xNew vNew) = delta <.> gradL
   where
    gradL =
      gradDeltaL
        (MkStepSize dt)
        (MkEquationParameter k gamma)
        (MkTestData x v)
        (MkTestData xNew vNew)
    delta = MkTestData (xNew .-. x) (vNew .-. v)

gradDeltaL
  (MkStepSize dt)
  (MkEquationParameter k gamma)
  (MkTestData x v)
  (MkTestData xNew vNew) = MkTestData dLdx dLdv
   where
    deltaX = xNew .-. x
    dxdt = deltaX ./ dt
    deltaV = vNew .-. v
    dvdt = deltaV ./ dt
    dLdx = (k ./. 2) *. (xNew .+. x) .+. (dxdt .-. (vNew .+. v) ./ 2)
    dLdv = dvdt ./ dt .+. ((k ./. 2) *. (xNew .+. x) .+. (gamma ./. 2) *. (vNew .+. v))

instance (Member (Reader (EquationParameter)) sig, Algebra sig m) => HasEquationParameterM m EquationParameter where
  getEquationParameterM = ask

instance (Algebra sig m, Member (State StateTest) sig, Member (Reader EquationParameter) sig, Member (Reader (StepSize Double)) sig) => HasObjectiveFunctionM m StateTest where
  getObjectiveFunctionM = do
    dt <- ask
    eqParam <- getEquationParameterM
    state <- get
    return $ deltaL dt eqParam state

instance (Algebra sig m, Member (State StateTest) sig, Member (Reader EquationParameter) sig, Member (Reader (StepSize Double)) sig) => HasGradObjectiveFunctionM m StateTest where
  getGradientOfObjectiveFunctionM = do
    dt <- ask
    eqParam <- getEquationParameterM
    state <- get
    return $ gradDeltaL dt eqParam state

instance (Algebra sig m) => HasInitialConditionM m StateTest where
  getInitialConditionM = do
    return $ MkTestData 1 0