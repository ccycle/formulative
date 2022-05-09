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
  { position :: Vector 3 Double
  , momentum :: Vector 3 Double
  }
  deriving stock (Show, Generic)
  deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace)
  deriving anyclass (DefaultOrdered, ExportRecordToFiles, FromLazyFields)

----------------------------------------------------------------
-- User-defined data for setting
----------------------------------------------------------------
data MyEquationConstants = MyEquationConstants
  { m :: Double
  , g :: Double
  , a :: Double
  , b :: Double
  , xInit :: Double
  , yInit :: Double
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
-- define system equation
----------------------------------------------------------------

----------------------------------------------------------------
---- initial condition
----------------------------------------------------------------
instance (Has (Reader MyEquationConstants) sig m) => HasInitialConditionM m MyVariable where
  getInitialConditionM = do
    MyEquationConstants{..} <- ask
    let zInit = xInit .^ 2 ./. (a .^ 2) .+. yInit .^ 2 ./. (b .^ 2)
    let x0 = generate f1
        f1 0 = xInit
        f1 1 = yInit
        f1 2 = zInit
        f1 _ = 0
    let pzInit = 2 .*. pxInit .*. xInit ./. (a .^ 2) .+. 2 .*. pyInit .*. yInit ./. (b .^ 2)
    let p0 = generate f2
        f2 0 = pxInit
        f2 1 = pyInit
        f2 2 = pzInit
        f2 _ = 0
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
    -- H = p^2/(2m) + gz
    dHdx = generate f
    f 2 = g
    f _ = 0
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
newtype MyConstraintCondition = MyConstraintCondition {l1 :: Double}
  deriving stock (Generic, Show)
  deriving anyclass (Additive, AdditiveGroup, VectorSpace, InnerProductSpace, NormSpace)

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
    dg1 = generate f
    f 0 = 2 .*. x ./. (a .^ 2)
    f 1 = 2 .*. y ./. (b .^ 2)
    f 2 = -1
    f _ = 0

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
-- export quantities
----------------------------------------------------------------

----------------------------------------------------------------
---- dependent parameter
----------------------------------------------------------------
kFunc MyEquationConstants{..} MyVariable{..} =
  let x = position `index` 0
      y = position `index` 1
      px = momentum `index` 0
      py = momentum `index` 1
   in ((a .*. b) .^ 2 ./. 8)
        .*. (1 .+. (2 .*. x ./. a .^ 2) .^ 2 .+. (2 .*. y ./. b .^ 2) .^ 2)
        .*. (g .-. (2 .*. px ./. (m .*. a)) .^ 2 .+. (2 .*. py ./. (m .*. b)) .^ 2)

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
        alphaPlusBeta = ((a .^ 2 .+. b .^ 2) ./. 2 .+. (2 .*. h0) ./. g) ./. 2
        alphaBeta = 2 .*. k0 ./. g
        alpha = alphaPlusBeta .+. sqrt (alphaPlusBeta .^ 2 .-. alphaBeta)
        beta = alphaPlusBeta .-. sqrt (alphaPlusBeta .^ 2 .-. alphaBeta)
    return $ MyDependentParameter h0 k0 (a .^ 2) (b .^ 2) alpha beta alphaPlusBeta alphaBeta
   where
    hFunc MyEquationConstants{..} (MyVariable x p) =
      let z = x `index` 2
          eK = p <.> p ./. (2 .*. m)
          eP = g .*. z
       in eK .+. eP

----------------------------------------------------------------
---- global dependent variable
----------------------------------------------------------------
data MyGlobalDependentVariable = MyGlobalDependentVariable
  { kineticEnergy :: Double
  , potentialEnergy :: Double
  , h :: Double
  , k :: Double
  }
  deriving stock (Show, Generic)
  deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace)
  deriving anyclass (DefaultOrdered, ToRecord)

-- Reference:
-- A. Gray, A. Jones, and R. Rimmer, “Motion under gravity on a paraboloid,” Journal of Differential Equations, vol. 45, no. 2, pp. 168–181, Aug. 1982.

instance
  ( Algebra sig m
  , Member (Reader MyEquationConstants) sig
  , Member (ConstrainedSystem MyConstraintCondition) sig
  ) =>
  HasGlobalDependentVariableM m MyVariable
  where
  type GlobalDependentVariable MyVariable = MyGlobalDependentVariable
  globalDependentVariableM MyVariable{..} = do
    MyEquationConstants{..} <- ask
    let p = momentum
    let z = position `index` 2
    let eK = p <.> p ./. (2 .*. m)
    let eP = g .*. z
    return $
      MyGlobalDependentVariable
        { kineticEnergy = eK
        , potentialEnergy = eP
        , h = eK .+. eP
        , k = undefined
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
  deriving anyclass (DefaultOrdered, ExportRecordToFiles)
instance
  ( Algebra sig m
  , Member (ConstrainedSystem MyConstraintCondition) sig
  , Member (Reader MyEquationConstants) sig
  ) =>
  HasLocalDependentVariableM m MyVariable
  where
  type LocalDependentVariable MyVariable = MyLocalDependentVariable
  localDependentVariableM MyVariable{..} = do
    MyEquationConstants{..} <- ask
    (MyConstraintCondition l1) <- getLagrangianMultiplier
    let f x = ((x `index` 0) ./. a) .^ 2 .+. ((x `index` 1) ./. b) .^ 2 .-. (x `index` 2)
    let g1 = f position
    return $ MyLocalDependentVariable g1 l1

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
    . runInitialConditionM @MyVariable
    $ mainCalcDynamics @MyVariable @Double
