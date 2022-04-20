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
import Formulative.Calculation.Algebra.DiscreteVariation (symmetrizePoly)
import Formulative.Calculation.DifferentialEquation.Dynamics.Carrier
import Formulative.Calculation.DifferentialEquation.Dynamics.Effect
import Formulative.Calculation.DifferentialEquation.Types
import Formulative.Calculation.Internal.Class
import Formulative.Calculation.Internal.Setting
import Formulative.Calculation.Internal.Variable.Carrier
import Formulative.Calculation.Internal.Variable.Effect
import Formulative.Calculation.Optimization.Carrier
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
data MyVariable = MyVariable {x :: Double, y :: Double}
    deriving stock (Show, Generic)
    deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace, DefaultOrdered, ToNamedRecord, ToRecords)

----------------------------------------------------------------
-- User-defined data for setting
----------------------------------------------------------------
data MyEquationConstants = MyEquationConstants {mu :: Double, x0 :: Double, y0 :: Double}
    deriving stock (Show, Generic)
    deriving anyclass (ToDhall, FromDhall, Hashable, HasDefaultValue)

data MySetting = MySetting {optimization :: OptimizationParameters Double, dynamics :: DynamicParameterSetting Double, export :: ExportSetting, equation :: MyEquationConstants}
    deriving stock (Show, Generic)
    deriving anyclass (ToDhall, FromDhall, Hashable, HasDefaultValue)

----------------------------------------------------------------
-- export data
----------------------------------------------------------------
instance (Monad m) => HasDependentParameterM m MyVariable

data MyDependentVariableGlobal = MyDependentVariableGlobal {kineticEnergy :: Double, springEnergy :: Double}
    deriving stock (Show, Generic)
    deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace, DefaultOrdered, ToRecord, ToNamedRecord)

instance (Has (Reader MyEquationConstants) sig m) => HasDependentVariableGlobalM m MyVariable where
    type DependentVariableGlobalType MyVariable = MyDependentVariableGlobal
    dependentVariableGlobalM (MyVariable x y) = do
        MyEquationConstants{..} <- ask
        let xdot = mu *. (x .-. (x .^ 3) ./ 3 .-. y)
        let eK = xdot <.> xdot ./ 2
        let eS = x <.> x ./ 2
        return $
            MyDependentVariableGlobal
                { kineticEnergy = eK
                , springEnergy = eS
                }

newtype MyDependentVariableLocal = MyDependentVariableLocal {xdot :: Double}
    deriving stock (Show, Generic)
    deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace, DefaultOrdered, ToRecords, ToNamedRecord)
instance (Algebra sig m, Member (Reader MyEquationConstants) sig) => HasDependentVariableLocalM m MyVariable where
    type DependentVariableLocalType MyVariable = MyDependentVariableLocal
    dependentVariableLocalM (MyVariable x y) = do
        MyEquationConstants{..} <- ask
        let x' = mu *. (x .-. (x .^ 3) ./ 3 .-. y)
        return $ MyDependentVariableLocal x'

----------------------------------------------------------------
-- define system equation
----------------------------------------------------------------
-- xdot = y
-- ydot = mu (1-x^2)y+x
gradDeltaL
    (StepSize dt)
    MyEquationConstants{..}
    (MyVariable x y)
    (MyVariable xNew yNew) = MyVariable dLdx dLdy
      where
        dx = xNew .-. x
        dxdt = dx ./ dt
        dy = yNew .-. y
        dydt = dy ./ dt
        y' = symmetrizePoly 1 y yNew
        x' = symmetrizePoly 1 x xNew
        x3' = symmetrizePoly 3 x xNew
        -- f z = z .^ 3 .-. z
        dLdx = dxdt .-. mu *. (x' .-. x3' ./ 3 - y')
        dLdy = dydt .-. x' ./ mu

instance (Algebra sig m, Member (Variable MyVariable) sig, Member (Reader MyEquationConstants) sig, Member (Dynamics Double) sig) => HasGradObjectiveFunctionM m MyVariable where
    getGradientOfObjectiveFunctionM = do
        dt <- askStepSize
        eqParam <- ask
        gradDeltaL dt eqParam <$> getVariableOld

instance (Algebra sig m, Member (Variable MyVariable) sig, Member (Reader MyEquationConstants) sig, Member (Dynamics Double) sig) => HasObjectiveFunctionM m MyVariable

instance (Has (Reader MyEquationConstants) sig m) => HasInitialConditionM m MyVariable where
    getInitialConditionM = do
        x0' <- asks x0
        y0' <- asks y0
        return $ MyVariable x0' y0'

instance (HasUpdateWithOptimization sig m MyVariable, Member (Reader MyEquationConstants) sig) => HasUpdateM m MyVariable where
    updateM = updateWithOptimization

----------------------------------------------------------------
-- main
----------------------------------------------------------------
main :: IO ()
main =
    runM . runSomeException printSomeException
        . runSettingFileIO @MySetting
        . runExportIO ODE
        . runOptimizationIO @Double
        . runEquationConstantsIO @MyEquationConstants
        . runDynamicsIO @Double
        . runInitialConditionM @MyVariable
        $ mainCalculationDynamic @MyVariable @Double