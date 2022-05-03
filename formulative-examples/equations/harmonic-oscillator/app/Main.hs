{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Formulative.Postprocess.Export.Variable.Class
import Control.Effect.Sum
import Data.Csv (DefaultOrdered, ToField, ToNamedRecord, ToRecord)
import Data.Hashable
import Dhall
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Algebra.DiscreteVariation
import Formulative.Calculation.DifferentialEquation.Dynamics.Carrier
import Formulative.Calculation.DifferentialEquation.Dynamics.Effect
import Formulative.Calculation.DifferentialEquation.Types
import Formulative.Calculation.Internal.Class
import Formulative.Calculation.Internal.Setting
import Formulative.Calculation.Internal.Variable.Carrier
import Formulative.Calculation.Internal.Variable.Effect
import Formulative.Calculation.Optimization.Carrier
import Formulative.Calculation.Optimization.Setting
import Formulative.Calculation.Optimization.Update
import Formulative.Calculation.VectorSpace.Class
import Formulative.Postprocess.Export.Carrier
import Formulative.Postprocess.Export.Dynamics
import Formulative.Postprocess.Export.Types
import Formulative.Preprocess.DefaultValue
import Formulative.Preprocess.Exception
import Formulative.Preprocess.SettingFile.Carrier
import Formulative.Postprocess.Export.Variable.Local

----------------------------------------------------------------
-- User-defined variable
----------------------------------------------------------------
data MyVariable = MyVariable {position :: Double, momentum :: Double}
  deriving stock (Show, Generic)
  deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace)
  deriving anyclass (DefaultOrdered, ExportRecordToFiles)

----------------------------------------------------------------
-- User-defined data for setting
----------------------------------------------------------------
data MyEquationConstants = MyEquationConstants
  { m :: Double
  , k :: Double
  , gamma :: Double
  , x0 :: Double
  , p0 :: Double
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToDhall, FromDhall, Hashable, HasDefaultValue)

data MySetting = MySetting
  { optimization :: OptimizationSetting Double
  , export :: ExportSetting
  , dynamics :: DynamicsSetting Double
  , equation :: MyEquationConstants
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToDhall, FromDhall, HasDefaultValue, Hashable)

----------------------------------------------------------------
-- export quantities
----------------------------------------------------------------

----------------------------------------------------------------
---- dependent parameter
----------------------------------------------------------------
-- https://en.wikipedia.org/wiki/Harmonic_oscillator#Damped_harmonic_oscillator
data MyDependentParameter = MyDependentParameter
  { dampingRatio :: Double
  , omega0 :: Double
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToDhall, FromDhall, Hashable)

instance (Has (Reader MyEquationConstants) sig m) => HasDependentParameterM m MyVariable where
  type DependentParameterType MyVariable = MyDependentParameter
  dependentParameterM = do
    MyEquationConstants{..} <- ask
    return
      MyDependentParameter
        { dampingRatio = gamma ./. (2 .*. sqrt (m .*. k))
        , omega0 = sqrt (k ./. m)
        }

----------------------------------------------------------------
---- global dependent variable
----------------------------------------------------------------
data MyGlobalDependentVariable = MyGlobalDependentVariable
  { kineticEnergy :: Double
  , potentialEnergy :: Double
  , lagrangian :: Double
  , hamiltonian :: Double
  , dHdt :: Double
  , power :: Double
  , totalChange :: Double
  }
  deriving stock (Show, Generic)
  deriving anyclass (DefaultOrdered, ToRecord, ToNamedRecord)

instance
  ( Has (Reader MyEquationConstants) sig m
  , Member (Variable MyVariable) sig
  , Member (Dynamics Double) sig
  ) =>
  HasGlobalDependentVariableM m MyVariable
  where
  type GlobalDependentVariable MyVariable = MyGlobalDependentVariable
  globalDependentVariableM (MyVariable x p) = do
    (MyVariable xOld pOld) <- getVariableOld
    MyEquationConstants{..} <- ask
    (StepSize dt) <- askStepSize
    let eK p = p <.> p ./. (2 .*. m)
    let eP x = (k ./. 2) .*. x .^ 2
    let h x p = eK p .+. eP x
    let dH = h x p .-. h xOld pOld
    let dW = (x .-. xOld) <.> (gamma ./. m) *. ((p .+. pOld) ./ 2)
    return $
      MyGlobalDependentVariable
        { kineticEnergy = eK p
        , potentialEnergy = eP x
        , lagrangian = eK p .-. eP x
        , hamiltonian = h x p
        , dHdt = dH ./. dt
        , power = dW ./. dt
        , totalChange = dH ./. dt .+. dW ./. dt
        }

----------------------------------------------------------------
---- local dependent variable
----------------------------------------------------------------
-- no data (default)
instance (Monad m) => HasLocalDependentVariableM m MyVariable

----------------------------------------------------------------
-- define system equation
----------------------------------------------------------------
gradDeltaL
  (StepSize dt)
  MyEquationConstants{..}
  (MyVariable xOld pOld)
  (MyVariable xNew pNew) = MyVariable dLdx dLdp
   where
    dxdt = (xNew .-. xOld) ./ dt
    dpdt = (pNew .-. pOld) ./ dt
    p = (pNew .+. pOld) ./ 2
    x = (xNew .+. xOld) ./ 2
    dLdx = dxdt .-. p ./ m
    dLdp = dpdt .+. (k *. x .+. (gamma ./. m) *. p)
instance
  ( Algebra sig m
  , Member (Variable MyVariable) sig
  , Member (Reader MyEquationConstants) sig
  , Member (Dynamics Double) sig
  ) =>
  HasGradObjectiveFunctionM m MyVariable
  where
  getGradientOfObjectiveFunctionM = do
    dt <- askStepSize
    eqParam <- ask
    gradDeltaL dt eqParam <$> getVariableOld

instance
  ( Algebra sig m
  , Member (Variable MyVariable) sig
  , Member (Reader MyEquationConstants) sig
  , Member (Dynamics Double) sig
  ) =>
  HasObjectiveFunctionM m MyVariable
instance
  (Has (Reader MyEquationConstants) sig m) =>
  HasInitialConditionM m MyVariable
  where
  getInitialConditionM = do
    MyEquationConstants{..} <- ask
    return $ MyVariable x0 p0

instance
  ( HasUpdateWithOptimization sig m MyVariable
  , Member (Reader MyEquationConstants) sig
  ) =>
  HasUpdateM m MyVariable
  where
  updateM = updateWithOptimization

----------------------------------------------------------------
-- main
----------------------------------------------------------------
main :: IO ()
main =
  runM . runSomeException printSomeException
    . runSettingFileIO @MySetting
    . runExportIO
    . runOptimizationIO @Double
    . runEquationConstantsIO @MyEquationConstants
    . runDynamicsIO @Double
    . runInitialConditionM @MyVariable
    $ mainCalcDynamics @MyVariable @Double
