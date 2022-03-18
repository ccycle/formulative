{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

module Test.Dynamic.HamiltonianMechanics where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Effect.Sum
import Data.Csv (DefaultOrdered, ToField, ToNamedRecord, ToRecord)
import Data.Hashable
import Dhall
import GHC.Generics
import GHC.Natural
import OptDEC.Calculation.Algebra.Arithmetic.Class
import OptDEC.Calculation.DifferentialEquation.Parameter
import OptDEC.Calculation.DiscreteExteriorCalculus.Geometry (DimensionOfManifold (MkDimensionOfManifold))
import OptDEC.Calculation.Internal.Class
import OptDEC.Calculation.Optimization.LineSearch
import OptDEC.Calculation.Optimization.Parameter
import OptDEC.Calculation.Optimization.Update
import OptDEC.Calculation.VectorSpace.Class
import OptDEC.Postprocess.Export.Class
import OptDEC.Postprocess.Export.Class (runExport)
import OptDEC.Postprocess.Export.Dynamics
import OptDEC.Preprocess.Exception (printError, runSomeException)
import OptDEC.Preprocess.Label
import Test.Tasty

data StateTest = MkTestData {position :: Double, momentum :: Double}
  deriving stock (Show, Generic)
  deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace, ToRecord, ToNamedRecord)

data ConstantsOfSystem = MkConstantsOfSystem {mass :: Double, k :: Double, gamma :: Double}
  deriving stock (Show, Generic)
  deriving anyclass (ToDhall, FromDhall, Hashable)
data InitialConditionParameter = InitialConditionParameter {x0 :: Double, p0 :: Double}
  deriving stock (Show, Generic)
  deriving anyclass (ToDhall, FromDhall, Hashable)
data Setting = Setting {optimization :: OptimizationParameters Double, equation :: ConstantsOfSystem, initialCondition :: InitialConditionParameter}
  deriving stock (Show, Generic)
  deriving anyclass (ToDhall, FromDhall, Hashable)

data GlobalQuantityTest = MkGlobalQuantityTest {kineticEnergy :: Double, potentialEnergy :: Double, totalEnergy :: Double}
  deriving stock (Show, Generic)
  deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace, DefaultOrdered, ToRecord, ToNamedRecord)

instance (Has (Reader InitialConditionParameter) sig m, HasUpdateUnconstrainedSystem sig m StateTest) => HasUpdateM m StateTest where
  updateM = updateUnconstrainedSystem

instance (Monad m, HasConstantsOfSystemM m ConstantsOfSystem) => HasSubDataGlobalM m StateTest where
  type GlobalQuantity StateTest = GlobalQuantityTest
  globalQuantityM (MkTestData x p) = do
    MkConstantsOfSystem m k g <- getConstantsOfSystemM
    let eK = p .^ 2 / (2 *. m)
    let eP = 1 ./. 2 * k .*. x .^ 2
    let e = eK .+. eP
    return $ MkGlobalQuantityTest eK eP e

instance (Monad m) => HasSubDataLocalM m StateTest

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

instance (Algebra sig m, Member (State StateTest) sig, Member (Reader ConstantsOfSystem) sig, Member (Dynamics Double) sig) => HasObjectiveFunctionM m StateTest where
  getObjectiveFunctionM = do
    dt <- askStepSize
    eqParam <- getConstantsOfSystemM
    deltaL dt eqParam <$> get

instance (Algebra sig m, Member (State StateTest) sig, Member (Reader ConstantsOfSystem) sig, Member (Dynamics Double) sig) => HasGradObjectiveFunctionM m StateTest where
  getGradientOfObjectiveFunctionM = do
    dt <- askStepSize
    eqParam <- getConstantsOfSystemM
    gradDeltaL dt eqParam <$> get

instance (Has (Reader InitialConditionParameter) sig m) => HasInitialConditionM m StateTest where
  getInitialConditionM = do
    x0' <- asks x0
    p0' <- asks p0
    return $ MkTestData x0' p0'

calcTest :: forall sig m. (Monad m, HasInitialConditionM m StateTest, Has (Lift IO) sig m) => m ()
calcTest = do
  x <- getInitialConditionM
  sendIO $ print (2 *. x :: StateTest)

execTest :: IO ()
execTest = runM . runReader (InitialConditionParameter 1 0) . (runInitialConditionM @StateTest) $ calcTest

-- calcDynamicTest ::
--   ( HasInitialConditionM m StateTest
--   , Member (State StateTest) sig
--   , HasUpdateUnconstrainedSystem sig m StateTest
--   , HasWriteVariableDynamic sig m StateTest
--   , Monad m
--   -- , HasMainCalculationDynamic sig m StateTest
--   , Member (Reader (StepSize Double)) sig
--   , Member (Reader LabelsOfSubGlobalQuantity) sig
--   , Member (Reader ConstantsOfSystem) sig
--   ) =>
--   m ()
calcDynamicTest :: IO ()
calcDynamicTest =
  runM . runSomeException printError
    . runExport (MkDimensionOfManifold 0) defaultExportSetting
    . runOptimization (defaultOptimizationParameters @Double)
    . runReader (MkConstantsOfSystem 1 1 1)
    . runReader (InitialConditionParameter 1 0)
    . runDynamics (defaultDynamicParameterSetting @Double)
    . runInitialConditionM @StateTest
    $ (mainCalculationDynamic @StateTest @Double)