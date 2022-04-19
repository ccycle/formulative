{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Effect.Sum
import Data.Csv hiding (index)
import Data.Hashable
import Data.Vector.Sized
import qualified Data.Vector.Sized as VS
import Dhall hiding (Vector)
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Algebra.DiscreteVariation (symmetrizePoly)
import Formulative.Calculation.DifferentialEquation.Dynamics.Carrier
import Formulative.Calculation.DifferentialEquation.Dynamics.Effect
import Formulative.Calculation.DifferentialEquation.Types
import Formulative.Calculation.Internal.Class
import Formulative.Calculation.Internal.Setting
import Formulative.Calculation.Internal.Variable.Carrier
import Formulative.Calculation.Internal.Variable.Effect
import Formulative.Calculation.Optimization.AugmentedLagrangian
import Formulative.Calculation.Optimization.Carrier
import Formulative.Calculation.Optimization.Constrained.Carrier (runConstrainedSystem, runConstrainedSystemIO)
import Formulative.Calculation.Optimization.Constrained.Effect
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
import GHC.Generics
import GHC.Natural

----------------------------------------------------------------
-- User-defined variable
----------------------------------------------------------------
data MyVariable = MyVariable {x :: Vector 3 Double, p :: Vector 3 Double}
  deriving stock (Show, Generic)
  deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace, DefaultOrdered, ToRecords)

----------------------------------------------------------------
-- User-defined data for setting
----------------------------------------------------------------
data MyEquationConstants = MyEquationConstants {m :: Double, g :: Double, a :: Double, b :: Double, xinit :: Double, yinit :: Double, pxInit :: Double, pyInit :: Double}
  deriving stock (Show, Generic)
  deriving anyclass (ToDhall, FromDhall, Hashable, HasDefaultValue)

data MySetting = MySetting {optimization :: OptimizationParameters Double, constrainedSystem :: ConstrainedSystemParameter Double, dynamics :: DynamicParameterSetting Double, export :: ExportSetting, equation :: MyEquationConstants}
  deriving stock (Show, Generic)
  deriving anyclass (ToDhall, FromDhall, Hashable, HasDefaultValue)

----------------------------------------------------------------
-- export quantities
----------------------------------------------------------------
instance (Monad m) => HasDependentParameterM m MyVariable
data MyDependentVariableGlobal = MyDependentVariableGlobal {kineticEnergy :: Double, potentialEnergy :: Double, lagrangian :: Double, hamiltonian :: Double}
  deriving stock (Show, Generic)
  deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace, DefaultOrdered, ToRecord, ToNamedRecord)
instance (Algebra sig m, Member (Reader MyEquationConstants) sig, Member (ConstrainedSystem MyConstraintCondition) sig) => HasDependentVariableGlobalM m MyVariable where
  type DependentVariableGlobalType MyVariable = MyDependentVariableGlobal
  dependentVariableGlobalM MyVariable{..} = do
    MyEquationConstants{..} <- ask
    let eK = p <.> p ./ (2 *. m)
    let eP = g .*. (x `index` 2)
    let e = eK .+. eP
    let l = eK .-. eP
    return $
      MyDependentVariableGlobal
        { kineticEnergy = eK
        , potentialEnergy = eP
        , lagrangian = l
        , hamiltonian = e
        }

data MyDependentVariableLocal = MyDependentVariableLocal {constraintCondition :: Double, lagrangeMultiplierPosition :: Double}
  deriving stock (Show, Generic)
  deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace, DefaultOrdered, ToRecord, ToNamedRecord, ToRecords)
instance (Algebra sig m, Member (ConstrainedSystem MyConstraintCondition) sig, Member (Reader MyEquationConstants) sig) => HasDependentVariableLocalM m MyVariable where
  type DependentVariableLocalType MyVariable = MyDependentVariableLocal
  dependentVariableLocalM MyVariable{..} = do
    MyEquationConstants{..} <- ask
    LagrangianMultiplier (MyConstraintCondition l1) <- getLagrangianMultiplier
    g1 <- constraintCondition1 x
    return $ MyDependentVariableLocal g1 l1

constraintCondition1 x = do
  MyEquationConstants{..} <- ask
  return $ ((x `index` 0) ./ a) .^ 2 .+. ((x `index` 1) ./ b) .^ 2 - (x `index` 2)

----------------------------------------------------------------
-- define system equation
----------------------------------------------------------------
gradDeltaL
  (StepSize dt)
  MyEquationConstants{..}
  (MyVariable x p)
  (MyVariable xNew pNew) = MyVariable dLdx dLdp
   where
    dx = xNew .-. x
    dxdt = dx ./ dt
    dp = pNew .-. p
    dpdt = dp ./ dt
    dHdx = zero VS.// [(2, g)] -- H = p^2/(2m) + gz
    p' = symmetrizePoly 1 pNew p
    dHdp = p' ./ m
    dLdx = dxdt .-. dHdp
    dLdp = dpdt .+. dHdx
instance (Algebra sig m, Member (Variable MyVariable) sig, Member (Reader MyEquationConstants) sig, Member (Dynamics Double) sig) => HasGradObjectiveFunctionM m MyVariable where
  getGradientOfObjectiveFunctionM = do
    dt <- askStepSize
    eqParam <- ask
    x <- getVariableOld
    return $ gradDeltaL dt eqParam x

equalityConstraint
  MyEquationConstants{..}
  (MyVariable xOld pOld)
  (MyVariable xNew pNew) = MyConstraintCondition g1
   where
    f1 x = ((x `index` 0) ./ a) .^ 2 .+. ((x `index` 1) ./ b) .^ 2 - (x `index` 2)
    g1 = f1 xNew .-. f1 xOld

gradConstraint
  MyEquationConstants{..}
  (MyConstraintCondition l1)
  (MyVariable xOld pOld)
  (MyVariable xNew pNew) = MyVariable zero (l1 *. dg1)
   where
    x = symmetrizePoly 1 xNew xOld `index` 0
    y = symmetrizePoly 1 xNew xOld `index` 1
    dg1 = zero VS.// [(0, 2 .*. x ./. (a .^ 2)), (1, 2 .*. y ./. (b .^ 2)), (2, -1)]

instance (Member (Reader MyEquationConstants) sig, Algebra sig m, Member (Variable MyVariable) sig) => HasGradPenaltyM m MyVariable where
  getGradPenaltyM = do
    c <- ask
    (varOld) <- getVariableOld
    return $ \(LagrangianMultiplier l) var -> gradConstraint c l varOld var

newtype MyConstraintCondition = MyConstraintCondition {l1 :: Double}
  deriving stock (Generic, Show)
  deriving anyclass (Additive, AdditiveGroup, VectorSpace, InnerProductSpace, NormSpace, FromDhall, ToDhall, DefaultOrdered, ToRecord, ToNamedRecord)
instance (Algebra sig m, Member (Variable MyVariable) sig, Member (Reader MyEquationConstants) sig) => HasEqualityConstraintM m MyVariable where
  type EqualityConstraintType MyVariable = MyConstraintCondition
  getEqualityConstraintM = do
    c <- ask
    (varOld) <- getVariableOld
    return $ \x -> equalityConstraint c varOld x

instance (Algebra sig m, Member (Variable MyVariable) sig, Member (Reader MyEquationConstants) sig, Member (Dynamics Double) sig) => HasObjectiveFunctionM m MyVariable
instance (Has (Reader MyEquationConstants) sig m) => HasInitialConditionM m MyVariable where
  getInitialConditionM = do
    MyEquationConstants{..} <- ask
    let x0' = zero; p0' = zero
    let x3init = xinit .^ 2 ./. (a .^ 2) .+. yinit .^ 2 ./. (b .^ 2)
    let p3init = 2 .*. pxInit .*. xinit ./. (a .^ 2) .+. 2 .*. pyInit .*. yinit ./. (b .^ 2)
    let x0 = x0' VS.// [(0, xinit), (1, yinit), (2, x3init)]
    let p0 = p0' VS.// [(0, pxInit), (1, pyInit), (2, p3init)]
    return $ MyVariable x0 p0

instance (HasUpdateWithConstrainedOptimization sig m MyVariable, Member (Reader MyEquationConstants) sig) => HasUpdateM m MyVariable where
  updateM = updateWithConstrainedOptimization

----------------------------------------------------------------
-- main
----------------------------------------------------------------
main :: IO ()
main =
  runM . runSomeException printSomeException
    . runSettingFileIO @MySetting
    . runExportIO ODE
    . runOptimizationIO @Double
    . runConstrainedSystemIO @MyConstraintCondition
    . runEquationConstantsIO @MyEquationConstants
    . runDynamicsIO @Double
    . runInitialConditionM @MyVariable
    $ mainCalculationDynamic @MyVariable @Double