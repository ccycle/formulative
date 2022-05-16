{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Effect.Sum
import Data.Hashable
import Data.Vector.Sized (Vector, generate, index)
import Dhall hiding (Vector)
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Algebra.DiscreteVariation (symmetrizePoly)
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
import Formulative.Calculation.Optimization.Constrained.AugmentedLagrangian
import Formulative.Calculation.Optimization.Constrained.Carrier (runConstrainedSystem, runConstrainedSystemIO)
import Formulative.Calculation.Optimization.Constrained.Class
import Formulative.Calculation.Optimization.Constrained.Effect
import Formulative.Calculation.Optimization.Constrained.Types
import Formulative.Calculation.Optimization.LineSearch
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
data MyVariable = MyVariable
  { position :: EuclideanCoord3d Double
  , velocity :: EuclideanCoord3d Double
  }
  deriving stock (Show, Generic)
  deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace)
  deriving anyclass (DefaultOrdered, ToVariableTypes, ToLazyFields, FromLazyFields)

----------------------------------------------------------------
-- User-defined data for setting
----------------------------------------------------------------
data MyEquationConstants = MyEquationConstants
  { g :: Double
  , a :: Double
  , b :: Double
  , xInit :: Double
  , yInit :: Double
  , vxInit :: Double
  , vyInit :: Double
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
-- define system equation
----------------------------------------------------------------

----------------------------------------------------------------
---- initial condition
----------------------------------------------------------------
instance (Has (Reader MyEquationConstants) sig m) => HasInitialConditionM m MyVariable where
  getInitialConditionM = do
    MyEquationConstants{..} <- ask
    let zInit = (xInit .^ 2 ./. (a .^ 2) .+. yInit .^ 2 ./. (b .^ 2)) ./. 2
        x0 = EuclideanCoord3d xInit yInit zInit
        vzInit = vxInit .*. xInit ./. (a .^ 2) .+. vyInit .*. yInit ./. (b .^ 2)
        v0 = EuclideanCoord3d vxInit vyInit vzInit
    return $ MyVariable x0 v0

----------------------------------------------------------------
---- equation
----------------------------------------------------------------
gradDeltaL
  (StepSize dt)
  MyEquationConstants{..}
  (MyVariable x v)
  (MyVariable xNew vNew) = MyVariable dLdx dLdp
   where
    dx = xNew .-. x
    dxdt = dx ./ dt
    dp = vNew .-. v
    dpdt = dp ./ dt
    -- H = v^2/(2m) + gz
    dHdx = EuclideanCoord3d 0 0 g
    f 2 = g
    f _ = 0
    v' = symmetrizePoly 1 vNew v
    dHdp = v'
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

instance
  ( Algebra sig m
  , Member (Variable MyVariable) sig
  , Member (Reader MyEquationConstants) sig
  , Member (Dynamics Double) sig
  ) =>
  HasObjectiveFunctionM m MyVariable

----------------------------------------------------------------
---- constraint conditions
----------------------------------------------------------------
newtype MyConstraintCondition = MyConstraintCondition {lagrangeMultiplier :: Double}
  deriving stock (Generic, Show)
  deriving anyclass (Additive, AdditiveGroup, VectorSpace, InnerProductSpace, NormSpace)
  deriving anyclass (DefaultOrdered, ToVariableTypes, ToLazyFields, FromLazyFields)

equalityConstraint
  MyEquationConstants{..}
  (MyVariable xOld vOld)
  (MyVariable xNew vNew) = MyConstraintCondition g1
   where
    f1 (EuclideanCoord3d x y z) = ((x) ./ a) .^ 2 .+. ((y) ./ b) .^ 2 - 2 .*. (z)
    g1 = f1 xNew .-. f1 xOld

----------------------------------------------------------------
---- gradient of constraint conditions
----------------------------------------------------------------
gradConstraint
  MyEquationConstants{..}
  (MyConstraintCondition l1)
  (MyVariable xOld vOld)
  (MyVariable xNew vNew) = MyVariable zero (l1 *. dg1)
   where
    xVec@(EuclideanCoord3d x y z) = symmetrizePoly 1 xNew xOld
    -- y = symmetrizePoly 1 xNew xOld `index` 1
    dg1 = EuclideanCoord3d (2 .*. x ./. (a .^ 2)) (2 .*. y ./. (b .^ 2)) (-2)

-- f 0 = 2 .*. x ./. (a .^ 2)
-- f 1 = 2 .*. y ./. (b .^ 2)
-- f 2 = -2
-- f _ = 0

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
    return $ \(LagrangeMultiplier l) var -> gradConstraint c l varOld var

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
  HasUpdateM m (MyVariable)
  where
  updateM = updateWithConstrainedOptimization

----------------------------------------------------------------
-- export quantities
----------------------------------------------------------------

----------------------------------------------------------------
---- dependent parameter
----------------------------------------------------------------
kFunc MyEquationConstants{..} (MyVariable EuclideanCoord3d{..} (EuclideanCoord3d vx vy _)) =
  (((a .*. b) .^ 2) ./. 2)
    .*. (1 .+. (x ./. a .^ 2) .^ 2 .+. (y ./. b .^ 2) .^ 2)
    .*. (g .-. ((vx ./. a) .^ 2 .+. (vy ./. b) .^ 2))

data MyDependentParameter = MyDependentParameter
  { h0 :: Double
  , k0 :: Double
  , aSquared :: Double
  , bSquared :: Double
  , alpha :: Double
  , beta :: Double
  , alphaPlusBeta :: Double
  , alphaBeta :: Double
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToDhall, FromDhall, Hashable, HasDefaultValue)
instance (Algebra sig m, Member (Reader MyEquationConstants) sig) => HasDependentParameterM m MyVariable where
  type DependentParameterType MyVariable = MyDependentParameter
  dependentParameterM = do
    x <- getInitialConditionM
    c@MyEquationConstants{..} <- ask
    let h0 = hFunc c x
        k0 = kFunc c x
        alphaPlusBeta = (a .^ 2 .+. b .^ 2) .+. (2 .*. h0) ./. g
        alphaBeta = 2 .*. k0 ./. g
        alpha = (alphaPlusBeta ./. 2) .-. sqrt ((alphaPlusBeta ./. 2) .^ 2 .-. alphaBeta)
        beta = (alphaPlusBeta ./. 2) .+. sqrt ((alphaPlusBeta ./. 2) .^ 2 .-. alphaBeta)
    return $ MyDependentParameter h0 k0 (a .^ 2) (b .^ 2) alpha beta alphaPlusBeta alphaBeta
   where
    hFunc MyEquationConstants{..} (MyVariable xVec@EuclideanCoord3d{..} v) =
      let eK = v <.> v ./. 2
          eP = g .*. z
       in eK .+. eP

----------------------------------------------------------------
---- global dependent variable
----------------------------------------------------------------
data MyGlobalDependentVariable = MyGlobalDependentVariable
  { kineticEnergy :: Double
  , potentialEnergy :: Double
  , hamiltonian :: Double
  , k :: Double
  }
  deriving stock (Show, Generic)
  deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace)
  deriving anyclass (DefaultOrdered, ToRecord)

-- Reference:
-- A. Gray, A. Jones, and R. Rimmer, “Motion under gravity on a paraboloid,” Journal of Differential Equations, vol. 45, no. 2, pp. 168–181, Aug. 1982, doi: 10.1016/0022-0396(82)90063-8.

instance
  ( Algebra sig m
  , Member (Reader MyEquationConstants) sig
  , Member (ConstrainedSystem MyConstraintCondition) sig
  ) =>
  HasGlobalDependentVariableM m MyVariable
  where
  type GlobalDependentVariable MyVariable = MyGlobalDependentVariable
  globalDependentVariableM (var@(MyVariable EuclideanCoord3d{..} v)) = do
    c@MyEquationConstants{..} <- ask
    let eK = v <.> v ./. 2
    let eP = g .*. z
    return $
      MyGlobalDependentVariable
        { kineticEnergy = eK
        , potentialEnergy = eP
        , hamiltonian = eK .+. eP
        , k = kFunc c var
        }

----------------------------------------------------------------
---- local dependent variable
----------------------------------------------------------------
newtype MyLocalDependentVariable = MyLocalDependentVariable
  { constraintCondition :: Double
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
  localDependentVariableM (MyVariable{..}) = do
    MyEquationConstants{..} <- ask
    let f EuclideanCoord3d{..} = ((x) ./. a) .^ 2 .+. ((y) ./. b) .^ 2 .-. 2 .*. (z)
    let g1 = f position
    return $ MyLocalDependentVariable g1

instance (HasExportDynamicsConstrained sig m MyVariable Double) => HasExportDynamicsM m MyVariable Double where
  exportDynamicsM = exportDynamicsConstrained

----------------------------------------------------------------
-- main
----------------------------------------------------------------
main :: IO ()
main =
  runM . runSomeException printSomeException
    . runSettingFileIO @MySetting
    . runExportIO
    . runOptimizationIO @Double
    . runConstrainedSystemIO @MyConstraintCondition
    . runEquationConstantsIO @MyEquationConstants
    . runDynamicsIO @Double
    . runInitialConditionM @(MyVariable)
    $ mainCalcDynamics @(MyVariable) @Double
