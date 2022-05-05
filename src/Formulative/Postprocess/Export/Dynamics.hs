{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RebindableSyntax #-}

module Formulative.Postprocess.Export.Dynamics (
    module Formulative.Postprocess.Export.Dynamics,
    module Data.Csv,
) where

import Control.Carrier.Lift
import Control.Effect.Error
import Control.Effect.Sum
import Control.Exception.Safe
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import Data.Csv (DefaultOrdered, ToField, ToRecord, encode)
import Data.String
import Dhall
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.DifferentialEquation.Dynamics.Effect
import Formulative.Calculation.DifferentialEquation.Types
import Formulative.Calculation.Internal.Class
import Formulative.Calculation.Internal.IfThenElse
import Formulative.Calculation.Internal.Variable.Effect
import Formulative.Postprocess.Export.Effect
import Formulative.Postprocess.Export.IO
import Formulative.Postprocess.Export.Types
import Formulative.Postprocess.Export.Variable.Global
import Formulative.Postprocess.Export.Variable.Local
import Formulative.Preprocess.IO
import Formulative.Preprocess.SettingFile.Effect
import Path
import Path.IO
import Prelude hiding (fromInteger)

exportDynamicParameter ::
    forall b sig m.
    ( Algebra sig m
    , ToField b
    , Member (Lift IO) sig
    , Member (Throw SomeException) sig
    , Member Export sig
    , Member (Dynamics b) sig
    ) =>
    IndexOfStep ->
    DynamicParameter b ->
    m ()
exportDynamicParameter (IndexOfStep i) (DynamicParameter t) = do
    ensureDirOutputM
    (OutputDir parentDir) <- askOutputDir
    (LabelOfDynamicParameter paramName) <- askLabelOfDynamicParameter @b
    name' <- liftEither $ parseRelFile paramName
    fileName <- liftEither $ replaceExtension ".csv" name'
    let x' = encode [(i, t)]
    let filePath = parentDir </> fileName
    flag <- sendIO $ doesFileExist filePath
    unless flag $
        sendIO $ BSL.writeFile (toFilePath filePath) (encode [("step number" :: String, paramName)])
    sendIO $ BSL.appendFile (toFilePath filePath) x'

exportLocalDependentVariableDynamics ::
    ( HasLocalDependentVariableM m a
    , Algebra sig m
    , Member (Throw SomeException) sig
    , Member (Lift IO) sig
    , Member Export sig
    , DefaultOrdered (LocalDependentVariable a)
    , ExportRecordToFiles (LocalDependentVariable a)
    ) =>
    IndexOfStep ->
    a ->
    m ()
exportLocalDependentVariableDynamics i x = do
    x' <- localDependentVariableM x
    exportRecordToFilesDynamicsM i x'

exportDynamicsM ::
    ( Algebra sig m
    , Member Export sig
    , Member (Lift IO) sig
    , Member (Dynamics b) sig
    , Member (Throw SomeException) sig
    , HasLocalDependentVariableM m a
    , HasGlobalDependentVariableM m a
    , DefaultOrdered a
    , DefaultOrdered (LocalDependentVariable a)
    , DefaultOrdered (GlobalDependentVariable a)
    , ToRecord (GlobalDependentVariable a)
    , ToField b
    , ExportRecordToFiles a
    , ExportRecordToFiles (LocalDependentVariable a)
    ) =>
    IndexOfStep ->
    DynamicParameter b ->
    a ->
    m ()
exportDynamicsM (IndexOfStep i) (DynamicParameter t) xi = do
    exportDynamicParameter (IndexOfStep i) (DynamicParameter t)
    exportRecordToFilesDynamicsM (IndexOfStep i) xi
    exportGlobalDependentVariable xi
    exportLocalDependentVariableDynamics (IndexOfStep i) xi

mainCalcDynamics ::
    forall a b sig m.
    ( Algebra sig m
    , Additive b
    , DefaultOrdered (GlobalDependentVariable a)
    , DefaultOrdered (LocalDependentVariable a)
    , DefaultOrdered a
    , HasDependentParameterM m a
    , HasGlobalDependentVariableM m a
    , HasLocalDependentVariableM m a
    , HasInitialConditionM m a
    , HasUpdateM m a
    , Member (Dynamics b) sig
    , Member (Lift IO) sig
    , Member (Throw SomeException) sig
    , Member (Variable a) sig
    , Member Export sig
    , Member SettingFile sig
    , Ord b
    , Semiring b
    , Show b
    , ToDhall (DependentParameterType a)
    , ToField b
    , ToRecord (GlobalDependentVariable a)
    , ExportRecordToFiles (LocalDependentVariable a)
    , ExportRecordToFiles a
    ) =>
    m ()
mainCalcDynamics = do
    preprocessM @m @a
    x <- getInitialConditionM @m @a
    DynamicsSetting{..} <- askDynamicsSetting @b
    msgStart
    go 0 interval maximumIterationNumber initialValue finalValue (StepSize stepSize) x x
  where
    go i nInterval iMax t finalVal (StepSize dt) xiMinus1 xi = do
        msgNewLine
        putStrLnM $ "step " ++ show i
        putStrLnM $ "parameter: " ++ show t
        when (i `mod` nInterval == 0) $ do
            putStrLnM "Exporting data.."
            exportDynamicsM (IndexOfStep i) (DynamicParameter t) xi
        if iMax <= i || 2 .*. finalVal <= (2 .*. t .+. dt) -- End condition
            then do
                msgEnd
                msgOutputDir
            else do
                putStrLnM "Updating variable.."
                putVariableOld xi
                xiPlus1 <- updateM xi
                putVariable xiPlus1
                go (succ i) nInterval iMax (t .+. dt) finalVal (StepSize dt) xi xiPlus1
