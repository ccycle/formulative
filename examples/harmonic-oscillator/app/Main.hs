{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Effect.Sum
import Data.Csv (DefaultOrdered, ToField, ToNamedRecord, ToRecord)
import Data.Hashable
import Dhall
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.DifferentialEquation.Dynamics.Carrier
import Formulative.Calculation.DifferentialEquation.Dynamics.Effect
import Formulative.Calculation.DifferentialEquation.Types
import Formulative.Calculation.Internal.Class
import Formulative.Calculation.Internal.Setting
import Formulative.Calculation.Optimization.Carrier
import Formulative.Calculation.Optimization.LineSearch
import Formulative.Calculation.Optimization.Parameter
import Formulative.Calculation.Optimization.Update
import Formulative.Calculation.VectorSpace.Class
import Formulative.Postprocess.Export.Carrier
import Formulative.Postprocess.Export.Dynamics
import Formulative.Postprocess.Export.ToRecords
import Formulative.Postprocess.Export.Types
import Formulative.Preprocess.DefaultValue
import Formulative.Preprocess.Exception
import Formulative.Preprocess.SettingFile.Carrier

----------------------------------------------------------------
-- User-defined variable
----------------------------------------------------------------
data MyVariable = MyVariable {position :: Double, momentum :: Double}
  deriving stock (Show, Generic)
  deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace, DefaultOrdered, ToNamedRecord, ToRecords)

----------------------------------------------------------------
-- User-defined Mysetting
----------------------------------------------------------------
data MyEquationConstants = MyEquationConstants {m :: Double, k :: Double, gamma :: Double, x0 :: Double, p0 :: Double}
  deriving stock (Show, Generic)
  deriving anyclass (ToDhall, FromDhall, Hashable, Additive)
instance DefaultValue MyEquationConstants where
  defaultValue = zero

data MySetting = MySetting {optimization :: OptimizationParameters Double, export :: ExportSetting, dynamics :: DynamicParameterSetting Double, equation :: MyEquationConstants}
  deriving stock (Show, Generic)
  deriving anyclass (ToDhall, FromDhall, DefaultValue, Hashable)

data MyDependentParameter = MyDependentParameter {dampingRatio :: Double, omega0 :: Double}
  deriving stock (Show, Generic)
  deriving anyclass (ToDhall, FromDhall, Hashable)
instance (Has (Reader MyEquationConstants) sig m) => HasDependentParameterM m MyVariable where
  type DependentParameterType MyVariable = MyDependentParameter
  dependentParameterM = do
    MyEquationConstants{..} <- ask
    return MyDependentParameter{dampingRatio = gamma / (2 * sqrt (m * k)), omega0 = sqrt (k / m)}

----------------------------------------------------------------
-- export quantoties
----------------------------------------------------------------
data MyDependentVariableGlobal = MyDependentVariableGlobal {kineticEnergy :: Double, potentialEnergy :: Double, lagrangian :: Double, hamiltonian :: Double}
  deriving stock (Show, Generic)
  deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace, DefaultOrdered, ToRecord, ToNamedRecord)

instance (Has (Reader MyEquationConstants) sig m) => HasDependentVariableGlobalM m MyVariable where
  type DependentVariableGlobalType MyVariable = MyDependentVariableGlobal
  dependentVariableGlobalM (MyVariable x p) = do
    MyEquationConstants{..} <- ask
    let eK = p .^ 2 / (2 *. m)
    let eP = 1 ./. 2 * k .*. x .^ 2
    let e = eK .+. eP
    let l = eK .-. eP
    return $
      MyDependentVariableGlobal
        { kineticEnergy = eK
        , potentialEnergy = eP
        , lagrangian = l
        , hamiltonian = e
        }

-- no data (default)
instance (Monad m) => HasDependentVariableLocalM m MyVariable

----------------------------------------------------------------
-- define system equation
----------------------------------------------------------------
gradDeltaL
  (MkStepSize dt)
  (MyEquationConstants{..})
  (MyVariable x p)
  (MyVariable xNew pNew) = MyVariable dLdx dLdp
   where
    dx = xNew .-. x
    dxdt = dx ./ dt
    dp = pNew .-. p
    dpdt = dp ./ dt
    dHdx = (k ./. 2) *. (xNew .+. x) .+. (gamma ./. (2 .*. m)) *. (pNew .+. p)
    dHdp = (pNew .+. p) ./ (2 .*. m)
    dLdx = dxdt .-. dHdp
    dLdp = dpdt .+. dHdx
instance (Algebra sig m, Member (State MyVariable) sig, Member (Reader MyEquationConstants) sig, Member (Dynamics Double) sig) => HasGradObjectiveFunctionM m MyVariable where
  getGradientOfObjectiveFunctionM = do
    dt <- askStepSize
    eqParam <- ask
    gradDeltaL dt eqParam <$> get

instance (Algebra sig m, Member (State MyVariable) sig, Member (Reader MyEquationConstants) sig, Member (Dynamics Double) sig) => HasObjectiveFunctionM m MyVariable
instance (Has (Reader MyEquationConstants) sig m) => HasInitialConditionM m MyVariable where
  getInitialConditionM = do
    x0' <- asks x0
    p0' <- asks p0
    return $ MyVariable x0' p0'

instance (HasUpdateUnconstrainedSystem sig m MyVariable, Member (Reader MyEquationConstants) sig) => HasUpdateM m MyVariable where
  updateM = updateUnconstrainedSystem

----------------------------------------------------------------
-- main
----------------------------------------------------------------
main :: IO ()
main =
  runM . runSomeException printSomeException
    . runSettingFileIO @MySetting
    . runExportIO ODE
    . runOptimization @Double defaultValue
    . runEquationConstantsIO @MyEquationConstants
    . runDynamicsIO @Double
    . runInitialConditionM @MyVariable
    $ mainCalculationDynamic @MyVariable @Double