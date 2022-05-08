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
import Data.Csv (DefaultOrdered, FromField, ToField, ToRecord)
import qualified Data.Csv.Streaming as Streaming
import Data.String
import Dhall
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.DifferentialEquation.Dynamics.Effect
import Formulative.Calculation.DifferentialEquation.Types
import Formulative.Calculation.Internal.Class
import Formulative.Calculation.Internal.IfThenElse
import Formulative.Calculation.Internal.List
import Formulative.Calculation.Internal.Variable.Effect
import Formulative.Postprocess.Export.CSV (encodeLF)
import Formulative.Postprocess.Export.Effect
import Formulative.Postprocess.Export.IO
import Formulative.Postprocess.Export.Path
import Formulative.Postprocess.Export.Types
import Formulative.Postprocess.Export.Variable.Global
import Formulative.Postprocess.Export.Variable.Local
import Formulative.Preprocess.CommandLineOptions
import Formulative.Preprocess.IO
import Formulative.Preprocess.ReadFile
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
    StepIndex ->
    DynamicParameter b ->
    m ()
exportDynamicParameter (StepIndex i) (DynamicParameter t) = do
    ensureDirOutputM
    (OutputDir parentDir) <- askOutputDir
    (LabelOfDynamicParameter paramName) <- askLabelOfDynamicParameter @b
    name' <- liftEither $ parseRelFile paramName
    fileName <- liftEither $ replaceExtension ".csv" name'
    let x' = encodeLF [(i, t)]
    let filePath = parentDir </> fileName
    flag <- sendIO $ doesFileExist filePath
    unless flag $
        sendIO $ BSL.writeFile (toFilePath filePath) (encodeLF [("step number" :: String, paramName)])
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
    StepIndex ->
    a ->
    m ()
exportLocalDependentVariableDynamics i x = do
    x' <- localDependentVariableM x
    localOutputDir addPostfixToDirForDependentVariable $ exportRecordToFilesDynamicsM i x'

exportDependentVariableDynamicsM i xi = do
    exportGlobalDependentVariable xi
    exportLocalDependentVariableDynamics i xi

exportDynamicsM i t xi = do
    exportDynamicParameter i t
    exportRecordToFilesDynamicsM i xi
    exportDependentVariableDynamicsM i xi

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
    , FromField b
    , Show b
    , ToDhall (DependentParameterType a)
    , ToField b
    , ToRecord (GlobalDependentVariable a)
    , ExportRecordToFiles (LocalDependentVariable a)
    , ExportRecordToFiles a
    , FromLazyFields a
    , Show a
    ) =>
    m ()
mainCalcDynamics = do
    preprocessM @m @a
    DynamicsSetting{..} <- askDynamicsSetting @b
    CmdOptions{..} <- sendIO cmdOptionIO
    msgNewLine
    msgStart
    msgNewLine
    x <- getInitialConditionM @m @a
    case recalculationOption of
        Continue -> do
            putStrLnM "RecalculationOption: Continue"
            putStrLnM "Read variable from files .."
            xs <- getLazyRecordsDynamicsM @a interval maximumIterationNumber
            -- putStrLnM $ show xs -- debug
            putStrLnM "Read dynamic parameter from files .."
            ts <- decodeDynamicParametersM @b
            -- putStrLnM $ show ts -- debug
            let MaxStepIndex iMax = maximumIterationNumber
            msgNewLine
            (i', t', x') <- goContinue iMax (StepIndex 0, initialValue) x ts xs
            putStrLnM "End loading cached files. "
            putStrLnM $ "Last step number: " <> show i'
            putStrLnM "Deleting temp files .."
            deleteTempDynamicParametersM @b
            deleteTempVariablesDynamicsM @a interval maximumIterationNumber
            msgNewLine
            putStrLnM "-- Continue calculation from last step --"
            go i' interval maximumIterationNumber t' finalValue stepSize x' x'
        NoOperation ->
            return ()
        Overwrite ->
            go (StepIndex 0) interval maximumIterationNumber initialValue finalValue stepSize x x
  where
    go i (IntervalStepIndex nInterval) (MaxStepIndex iMax) (DynamicParameter t) (DynamicParameter finalVal) (StepSize dt) xiMinus1 xi = do
        msgNewLine
        putStrLnM $ "step " ++ show i
        putStrLnM $ "parameter: " ++ show t
        when (i `mod` nInterval == StepIndex 0) $ do
            putStrLnM "Exporting data.."
            exportDynamicsM i (DynamicParameter t) xi
        if iMax <= i || 2 .*. finalVal <= (2 .*. t .+. dt) -- End condition
            then do
                msgNewLine
                msgEnd
                msgNewLine
                msgOutputDir
                msgNewLine
            else do
                putStrLnM "Updating variable.."
                putVariableOld xi
                xiPlus1 <- updateM xi
                putVariable xiPlus1
                go (succ i) (IntervalStepIndex nInterval) (MaxStepIndex iMax) (DynamicParameter (t .+. dt)) (DynamicParameter finalVal) (StepSize dt) xi xiPlus1
    goContinue iMax (i', t') x' (Streaming.Nil _ _) _ =
        return (i', t', x')
    goContinue iMax (i', t') x' _ [] =
        return (i', t', x')
    goContinue iMax (i', t') x' (t `Streaming.Cons` ts) (x : xs) =
        case (t, x) of
            (Right (i'', t''), Right x'') -> do
                putStrLnM "(Recalculation)"
                putStrLnM $ "step " ++ show i''
                putStrLnM $ "parameter: " ++ show t''
                putStrLnM "Exporting data from cached files.."
                unless (null xs || nullRecords ts) $
                    exportDynamicsM i'' t'' x''
                msgNewLine
                goContinue iMax (i'', t'') x'' ts xs
            (Left str, _) -> do
                putStrLnM str
                return (i', t', x')
            (_, Left e) -> do
                putStrLnM $ displayException e
                return (i', t', x')
