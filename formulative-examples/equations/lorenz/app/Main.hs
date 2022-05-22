{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Effect.Sum
import Data.Csv (DefaultOrdered, ToField, ToNamedRecord, ToRecord)
import Data.Hashable
import Dhall
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Algebra.DiscreteVariation
import Formulative.Calculation.Coordinates.Dim3.Euclidean
import Formulative.Calculation.DifferentialEquation.Dynamics.Carrier
import Formulative.Calculation.DifferentialEquation.Dynamics.Effect
import Formulative.Calculation.DifferentialEquation.Types
import Formulative.Calculation.Internal.Class
import Formulative.Calculation.Internal.Setting
import Formulative.Calculation.Internal.Variable.Carrier
import Formulative.Calculation.Internal.Variable.Effect
import Formulative.Calculation.Optimization.Carrier
import Formulative.Calculation.Optimization.Class
import Formulative.Calculation.Optimization.Types
import Formulative.Calculation.Optimization.Update
import Formulative.Calculation.VectorSpace.Class
import Formulative.Postprocess.Export.Carrier
import Formulative.Postprocess.Export.Dynamics
import Formulative.Postprocess.Export.Types
import Formulative.Postprocess.Export.Variable.Class
import Formulative.Postprocess.Export.Variable.Local
import Formulative.Preprocess.DefaultValue
import Formulative.Preprocess.Exception
import Formulative.Preprocess.ReadFile
import Formulative.Preprocess.SettingFile.Carrier

----------------------------------------------------------------
-- User-defined variable
----------------------------------------------------------------
data MyVariable = MyVariable {x :: Double, y :: Double, z :: Double}
  deriving stock (Show, Generic)
  deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace)
  deriving anyclass (DefaultOrdered, ToVariableTypes, ToLazyFields, FromLazyFields)

----------------------------------------------------------------
-- User-defined data for setting
----------------------------------------------------------------
data MyEquationConstants = MyEquationConstants
  { rho :: Double
  , beta :: Double
  , sigma :: Double
  , x0 :: Double
  , y0 :: Double
  , z0 :: Double
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
  { fixedPointX :: Double
  , fixedPointY :: Double
  , fixedPointZ :: Double
  , rhoH :: Double
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToDhall, FromDhall, Hashable)
instance (Has (Reader MyEquationConstants) sig m) => HasDependentParameterM m MyVariable where
  type DependentParameterType MyVariable = MyDependentParameter
  dependentParameterM = do
    MyEquationConstants{..} <- ask
    return
      MyDependentParameter
        { fixedPointX = if rho .-. 1 < 0 then sqrt (beta .*. (rho .-. 1)) else 0
        , fixedPointY = if rho .-. 1 < 0 then sqrt (beta .*. (rho .-. 1)) else 0
        , fixedPointZ = if rho .-. 1 < 0 then rho .-. 1 else 0
        , rhoH = sigma .*. (sigma .+. beta .+. 3) ./. (sigma .-. beta .-. 1)
        }

----------------------------------------------------------------
---- global dependent variable
----------------------------------------------------------------
-- no output
instance
  ( Has (Reader MyEquationConstants) sig m
  ) =>
  HasGlobalDependentVariableM m MyVariable

----------------------------------------------------------------
---- local dependent variable
----------------------------------------------------------------
newtype MyLocalDependentVariable = MyLocalDependentVariable
  { velocity :: EuclideanCoord3d Double
  }
  deriving stock (Show, Generic)
  deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace)
  deriving anyclass (DefaultOrdered, ToVariableTypes, ToLazyFields, FromLazyFields)
instance
  ( Algebra sig m
  , Member (Reader MyEquationConstants) sig
  ) =>
  HasLocalDependentVariableM m MyVariable
  where
  type LocalDependentVariable MyVariable = MyLocalDependentVariable
  localDependentVariableM MyVariable{..} = do
    MyEquationConstants{..} <- ask
    let xDot = sigma .*. (y .-. x)
        yDot = x .*. (rho .-. z) .-. y
        zDot = x .*. y .-. beta .*. z
    return $ MyLocalDependentVariable (EuclideanCoord3d xDot yDot zDot)

instance (HasExportDynamicsUnconstrained sig m MyVariable Double) => HasExportDynamicsM m MyVariable Double where
  exportDynamicsM = exportDynamicsUnconstrained

----------------------------------------------------------------
-- define system equation
----------------------------------------------------------------
gradDeltaL
  (StepSize dt)
  MyEquationConstants{..}
  xOld
  xNew = dLdX
   where
    xVecDot = (xNew .-. xOld) ./ dt
    (MyVariable x y z) = (xNew .+. xOld) ./ 2
    xDot = sigma .*. (y .-. x)
    yDot = x .*. (rho .-. z) .-. y
    zDot = x .*. y .-. beta .*. z
    dLdX = xVecDot .-. MyVariable xDot yDot zDot

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
    return $ MyVariable x0 y0 z0

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
