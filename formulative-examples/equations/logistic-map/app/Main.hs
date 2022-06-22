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
import Formulative.Calculation.Algebra.Arithmetic
import Formulative.Calculation.DifferentialEquation.Dynamics.Carrier
import Formulative.Calculation.DifferentialEquation.Dynamics.Effect
import Formulative.Calculation.DifferentialEquation.Types
import Formulative.Calculation.Internal.Class
import Formulative.Calculation.Internal.Setting
import Formulative.Calculation.Internal.Variable.Carrier
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
import GHC.Natural

----------------------------------------------------------------
-- User-defined variable
----------------------------------------------------------------
newtype MyVariable = MyVariable {x :: Double}
  deriving stock (Show, Generic)
  deriving anyclass (Additive, AdditiveGroup, VectorSpace, NormSpace, InnerProductSpace)
  deriving anyclass (DefaultOrdered, ToVariableTypes, ToLazyFields, FromLazyFields)

----------------------------------------------------------------
-- User-defined data for setting
----------------------------------------------------------------
data MyEquationConstants = MyEquationConstants
  { r :: Double
  , x0 :: Double
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToDhall, FromDhall, Hashable, HasDefaultValue)

data MySetting = MySetting
  { export :: ExportSetting
  , dynamics :: DynamicsSetting Natural
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
-- no output
instance (Has (Reader MyEquationConstants) sig m) => HasDependentParameterM m MyVariable

----------------------------------------------------------------
---- global dependent variable
----------------------------------------------------------------
instance
  ( Has (Reader MyEquationConstants) sig m
  ) =>
  HasGlobalDependentVariableM m MyVariable

----------------------------------------------------------------
---- local dependent variable
----------------------------------------------------------------

instance
  ( Algebra sig m
  , Member (Reader MyEquationConstants) sig
  ) =>
  HasLocalDependentVariableM m MyVariable

instance (HasExportDynamicsUnconstrained sig m MyVariable Natural) => HasExportDynamicsM m MyVariable Natural where
  exportDynamicsM = exportDynamicsUnconstrained

----------------------------------------------------------------
-- define system equation
----------------------------------------------------------------

instance
  (Has (Reader MyEquationConstants) sig m) =>
  HasInitialConditionM m MyVariable
  where
  getInitialConditionM = do
    MyEquationConstants{..} <- ask
    return $ MyVariable x0

logisticMap MyEquationConstants{..} MyVariable{..} = MyVariable (r .*. x .*. (1 .-. x))

instance
  ( Algebra sig m
  , Member (Reader MyEquationConstants) sig
  ) =>
  HasUpdateM m MyVariable
  where
  updateM x = do
    c <- ask
    return $ logisticMap c x

----------------------------------------------------------------
-- main
----------------------------------------------------------------
main :: IO ()
main =
  runM . runSomeException printSomeException
    . runSettingFileIO @MySetting
    . runExportIO
    . runEquationConstantsIO @MyEquationConstants
    . runDynamicsIO @Natural
    . runInitialConditionM @MyVariable
    $ mainCalcDynamics @MyVariable @Natural
