{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Test.Dynamic.HamiltonianMechanics where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Effect.Sum
import Data.Csv (ToField, ToRecord)
import GHC.Generics
import GHC.Natural
import OptDEC.Calculation.Algebra.Arithmetic.Class
import OptDEC.Calculation.DifferentialEquation.Parameter
import OptDEC.Calculation.Internal.Class
import OptDEC.Calculation.Optimization.LineSearch
import OptDEC.Calculation.Optimization.Update
import OptDEC.Calculation.VectorSpace.Class
import OptDEC.Postprocess.Export.Class
import Test.Tasty

data StateTest = MkTestData {position :: Double, momentum :: Double}
  deriving stock (Show, Generic)
  deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace, ToRecord, ToField)

data ConstantsOfSystem = MkConstantsOfSystem {mass :: Double, k :: Double, gamma :: Double}

data GlobalQuantityTest = MkGlobalQuantityTest {kineticEnergy :: Double, potentialEnergy :: Double, totalEnergy :: Double}

instance (HasUpdateUnconstrainedSystem sig m StateTest) => HasUpdateM m StateTest where
  updateM = updateUnconstrainedSystem

instance (Monad m, HasConstantsOfSystemM m ConstantsOfSystem) => HasSubDataGlobalM m StateTest where
  type GlobalQuantity StateTest = GlobalQuantityTest
  globalQuantityM (MkTestData x p) = do
    MkConstantsOfSystem m k g <- getConstantsOfSystemM
    let eK = p .^ 2 / (2 *. m)
    let eP = 1 ./. 2 * k .*. x .^ 2
    let e = eK .+. eP
    return $ MkGlobalQuantityTest eK eP e

deltaL
  (MkStepSize dt)
  (MkConstantsOfSystem mass k gamma)
  (MkTestData x p)
  (MkTestData xNew pNew) = delta <.> gradL
   where
    gradL =
      gradDeltaL
        (MkStepSize dt)
        (MkConstantsOfSystem mass k gamma)
        (MkTestData x p)
        (MkTestData xNew pNew)
    delta = MkTestData (xNew .-. x) (pNew .-. p)

gradDeltaL
  (MkStepSize dt)
  (MkConstantsOfSystem mass k gamma)
  (MkTestData x p)
  (MkTestData xNew pNew) = MkTestData dLdx dLdp
   where
    dx = xNew .-. x
    dxdt = dx ./ dt
    dp = pNew .-. p
    dpdt = dp ./ dt
    dLdx = dpdt .-. (k ./. 2) *. (xNew .+. x)
    dLdp = (-1) *. dxdt .-. ((k ./. 2) *. (xNew .+. x) .+. (gamma ./. 2) *. (pNew .+. p))

instance (Member (Reader ConstantsOfSystem) sig, Algebra sig m) => HasConstantsOfSystemM m ConstantsOfSystem where
  getConstantsOfSystemM = ask

instance (Algebra sig m, Member (State StateTest) sig, Member (Reader ConstantsOfSystem) sig, Member (Reader (StepSize Double)) sig) => HasObjectiveFunctionM m StateTest where
  getObjectiveFunctionM = do
    dt <- ask
    eqParam <- getConstantsOfSystemM
    deltaL dt eqParam <$> get

instance (Algebra sig m, Member (State StateTest) sig, Member (Reader ConstantsOfSystem) sig, Member (Reader (StepSize Double)) sig) => HasGradObjectiveFunctionM m StateTest where
  getGradientOfObjectiveFunctionM = do
    dt <- ask
    eqParam <- getConstantsOfSystemM
    gradDeltaL dt eqParam <$> get

instance (Monad m) => HasInitialConditionM m StateTest where
  getInitialConditionM =
    return $ MkTestData 1 0

instance (Monad m) => HasParameterConfigM m StateTest where
  -- type ParameterType StateTest = Double
  getParameterSettingM =
    return $ MkParameterSetting{initialValue = 0, finalValue = 1, stepSize = 0.1, maxIteration = 1000}

calcTest :: forall sig m. (Monad m, HasInitialConditionM m StateTest, Has (Lift IO) sig m) => m ()
calcTest = do
  x <- getInitialConditionM
  sendIO $ print (2 *. x :: StateTest)

execTest :: IO ()
execTest = runM . (runInitialConditionM @StateTest) $ calcTest

instance (HasWriteVariableDynamic sig m StateTest, HasWriteGlobalQuantity sig m StateTest) => ExportSubDataM m StateTest where
  exportSubDataM x = do
    writeGlobalQuantity x

-- instance HasMainCalculationDynamic StateTest Double sig m

calcDynamicTest ::
  ( HasInitialConditionM m StateTest
  , Member (State StateTest) sig
  , HasUpdateUnconstrainedSystem sig m StateTest
  , HasWriteVariableDynamic sig m StateTest
  , Monad m
  , HasMainCalculationDynamic sig m StateTest
  , Member (Reader (StepSize Double)) sig
  , Member (Reader MkLabelsOfSubGlobalQuantity) sig
  , Member (Reader ConstantsOfSystem) sig
  ) =>
  m ()
calcDynamicTest = runInitialConditionM @StateTest (mainCalculationDynamic @StateTest)