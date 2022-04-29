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
import Data.Vector.Sized (Vector, index, (//))
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
import Formulative.Calculation.Optimization.Carrier
import Formulative.Calculation.Optimization.Constrained.AugmentedLagrangian
import Formulative.Calculation.Optimization.Constrained.Carrier (runConstrainedSystem, runConstrainedSystemIO)
import Formulative.Calculation.Optimization.Constrained.Effect
import Formulative.Calculation.Optimization.LineSearch
import Formulative.Calculation.Optimization.Setting
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
data MyVariable = MyVariable
  { position :: Vector 3 Double
  , momentum :: Vector 3 Double
  }
  deriving stock (Show, Generic)
  deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace)
  deriving anyclass (DefaultOrdered, ToRecords)

----------------------------------------------------------------
-- User-defined data for setting
----------------------------------------------------------------
data MyEquationConstants = MyEquationConstants
  { m :: Double
  , g :: Double
  , a :: Double
  , b :: Double
  , xinit :: Double
  , yinit :: Double
  , pxInit :: Double
  , pyInit :: Double
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToDhall, FromDhall, Hashable, HasDefaultValue)

data MySetting = MySetting
  { optimization :: OptimizationSetting Double
  , constrainedSystem :: ConstrainedSystemSetting Double
  , dynamics :: DynamicsSetting Double
  , export :: ExportSetting
  , equation :: MyEquationConstants
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToDhall, FromDhall, Hashable, HasDefaultValue)

----------------------------------------------------------------
-- export quantities
----------------------------------------------------------------

----------------------------------------------------------------
---- dependent parameter
----------------------------------------------------------------
-- no output
instance (Monad m) => HasDependentParameterM m MyVariable

----------------------------------------------------------------
---- global dependent variable
----------------------------------------------------------------
data MyGlobalDependentVariable = MyGlobalDependentVariable
  { kineticEnergy :: Double
  , potentialEnergy :: Double
  , lagrangian :: Double
  , hamiltonian :: Double
  }
  deriving stock (Show, Generic)
  deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace)
  deriving anyclass (DefaultOrdered, ToRecord, ToNamedRecord)

instance
  ( Algebra sig m
  , Member (Reader MyEquationConstants) sig
  , Member (ConstrainedSystem MyConstraintCondition) sig
  ) =>
  HasGlobalDependentVariableM m MyVariable
  where
  type GlobalDependentVariable MyVariable = MyGlobalDependentVariable
  dependentVariableGlobalM MyVariable{..} = do
    MyEquationConstants{..} <- ask
    let p = momentum
    let z = position `index` 2
    let eK = p <.> p ./. (2 .*. m)
    let eP = g .*. z
    return $
      MyGlobalDependentVariable
        { kineticEnergy = eK
        , potentialEnergy = eP
        , lagrangian = eK .-. eP
        , hamiltonian = eK .+. eP
        }

----------------------------------------------------------------
---- local dependent variable
----------------------------------------------------------------
data MyLocalDependentVariable = MyLocalDependentVariable
  { constraintCondition :: Double
  , lagrangianMultiplier :: Double
  }
  deriving stock (Show, Generic)
  deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace)
  deriving anyclass (DefaultOrdered, ToRecords)
instance
  ( Algebra sig m
  , Member (ConstrainedSystem MyConstraintCondition) sig
  , Member (Reader MyEquationConstants) sig
  ) =>
  HasLocalDependentVariableM m MyVariable
  where
  type LocalDependentVariable MyVariable = MyLocalDependentVariable
  dependentVariableLocalM MyVariable{..} = do
    MyEquationConstants{..} <- ask
    (MyConstraintCondition l1) <- getLagrangianMultiplier
    g1 <- constraintCondition1 position
    return $ MyLocalDependentVariable g1 l1

----------------------------------------------------------------
-- define system equation
----------------------------------------------------------------

----------------------------------------------------------------
---- initial condition
----------------------------------------------------------------
instance
  ( Algebra sig m
  , Member (Variable MyVariable) sig
  , Member (Reader MyEquationConstants) sig
  , Member (Dynamics Double) sig
  ) =>
  HasObjectiveFunctionM m MyVariable
instance (Has (Reader MyEquationConstants) sig m) => HasInitialConditionM m MyVariable where
  getInitialConditionM = do
    MyEquationConstants{..} <- ask
    let x0' = zero; p0' = zero
    let x3init = xinit .^ 2 ./. (a .^ 2) .+. yinit .^ 2 ./. (b .^ 2)
    let p3init = 2 .*. pxInit .*. xinit ./. (a .^ 2) .+. 2 .*. pyInit .*. yinit ./. (b .^ 2)
    let x0 = x0' // [(0, xinit), (1, yinit), (2, x3init)]
    let p0 = p0' // [(0, pxInit), (1, pyInit), (2, p3init)]
    return $ MyVariable x0 p0

----------------------------------------------------------------
---- equation
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
    dHdx = zero // [(2, g)] -- H = p^2/(2m) + gz
    p' = symmetrizePoly 1 pNew p
    dHdp = p' ./ m
    dLdx = dxdt .-. dHdp
    dLdp = dpdt .+. dHdx
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

----------------------------------------------------------------
---- constraint conditions
----------------------------------------------------------------
newtype MyConstraintCondition = MyConstraintCondition {l1 :: Double}
  deriving stock (Generic, Show)
  deriving anyclass (Additive, AdditiveGroup, VectorSpace, InnerProductSpace, NormSpace)

constraintCondition1 x = do
  MyEquationConstants{..} <- ask
  return $ ((x `index` 0) ./. a) .^ 2 .+. ((x `index` 1) ./. b) .^ 2 .-. (x `index` 2)

equalityConstraint
  MyEquationConstants{..}
  (MyVariable xOld pOld)
  (MyVariable xNew pNew) = MyConstraintCondition g1
   where
    f1 x = ((x `index` 0) ./ a) .^ 2 .+. ((x `index` 1) ./ b) .^ 2 - (x `index` 2)
    g1 = f1 xNew .-. f1 xOld

----------------------------------------------------------------
---- gradient of constraint conditions
----------------------------------------------------------------
gradConstraint
  MyEquationConstants{..}
  (MyConstraintCondition l1)
  (MyVariable xOld pOld)
  (MyVariable xNew pNew) = MyVariable zero (l1 *. dg1)
   where
    x = symmetrizePoly 1 xNew xOld `index` 0
    y = symmetrizePoly 1 xNew xOld `index` 1
    dg1 = zero // [(0, 2 .*. x ./. (a .^ 2)), (1, 2 .*. y ./. (b .^ 2)), (2, -1)]

instance
  ( Algebra sig m
  , Member (Reader MyEquationConstants) sig
  , Member (Variable MyVariable) sig
  ) =>
  HasGradPenaltyM m MyVariable
  where
  getGradPenaltyM = do
    c <- ask
    varOld <- getVariableOld
    return $ \(LagrangianMultiplier l) var -> gradConstraint c l varOld var

instance
  ( Algebra sig m
  , Member (Variable MyVariable) sig
  , Member (Reader MyEquationConstants) sig
  ) =>
  HasEqualityConstraintM m MyVariable
  where
  type EqualityConstraintType MyVariable = MyConstraintCondition
  getEqualityConstraintM = do
    c <- ask
    varOld <- getVariableOld
    return $ \x -> equalityConstraint c varOld x

----------------------------------------------------------------
---- update function for constrained system
----------------------------------------------------------------
instance
  ( HasUpdateWithConstrainedOptimization sig m MyVariable
  , Member (Reader MyEquationConstants) sig
  ) =>
  HasUpdateM m MyVariable
  where
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
    $ mainCalcDynamics @MyVariable @Double