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
import Data.Proxy
import Data.String
import Dhall
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.DifferentialEquation.Dynamics.Effect
import Formulative.Calculation.DifferentialEquation.Types
import Formulative.Calculation.Internal.Class
import Formulative.Calculation.Internal.IfThenElse
import Formulative.Calculation.Internal.List
import Formulative.Calculation.Internal.Variable.Effect
import Formulative.Calculation.Optimization.Constrained.Class
import Formulative.Calculation.Optimization.Constrained.Effect (ConstrainedSystem, getLagrangeMultiplier)
import Formulative.Calculation.Optimization.Constrained.Types (EqualityConstraint (EqualityConstraint))
import Formulative.Postprocess.Export.CSV (encodeLF)
import Formulative.Postprocess.Export.Effect
import Formulative.Postprocess.Export.IO
import Formulative.Postprocess.Export.Path
import Formulative.Postprocess.Export.Types
import Formulative.Postprocess.Export.Variable.Class
import Formulative.Postprocess.Export.Variable.Global
import Formulative.Postprocess.Export.Variable.Local
import Formulative.Preprocess.CommandLineOptions
import Formulative.Preprocess.IO
import Formulative.Preprocess.ReadFile
import Formulative.Preprocess.SettingFile.Effect
import Path
import Path.IO
import Prelude hiding (fromInteger)

class HasExportDynamicsM m a b where
    exportDynamicsM :: StepIndex -> DynamicParameter b -> a -> m ()

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

exportVariableDynamics ::
    forall sig m a.
    ( Algebra sig m
    , Member (Throw SomeException) sig
    , Member (Lift IO) sig
    , Member Export sig
    , DefaultOrdered a
    , ToLazyFields a
    , ToVariableTypes a
    ) =>
    StepIndex ->
    a ->
    m ()
exportVariableDynamics i xi = do
    msgExportFile (Proxy @a)
    exportRecordToFilesDynamicsM i xi

exportLocalDependentVariableDynamics ::
    forall sig m a.
    ( HasLocalDependentVariableM m a
    , Algebra sig m
    , Member (Throw SomeException) sig
    , Member (Lift IO) sig
    , Member Export sig
    , DefaultOrdered (LocalDependentVariable a)
    , ToLazyFields (LocalDependentVariable a)
    , ToVariableTypes (LocalDependentVariable a)
    ) =>
    StepIndex ->
    a ->
    m ()
exportLocalDependentVariableDynamics i x = do
    x' <- localDependentVariableM x
    -- OutputDir dir <- askOutputDir
    -- putStrLnWithLogPathM
    localOutputDir addPostfixToDirForDependentVariable $ do
        -- putStrLnWithLogPathM dir String
        exportVariableDynamics i x'

exportDependentVariableDynamicsM i xi = do
    exportGlobalDependentVariable xi
    exportLocalDependentVariableDynamics i xi

type HasExportDynamicsUnconstrained sig m a b =
    ( Algebra sig m
    , ToField b
    , Member (Dynamics b) sig
    , Member (Lift IO) sig
    , Member (Throw SomeException) sig
    , Member Export sig
    , HasLocalDependentVariableM m a
    , ToRecord (GlobalDependentVariable a)
    , HasGlobalDependentVariableM m a
    , DefaultOrdered a
    , DefaultOrdered (GlobalDependentVariable a)
    , DefaultOrdered (LocalDependentVariable a)
    , ToLazyFields a
    , ToLazyFields (LocalDependentVariable a)
    , ToVariableTypes a
    , ToVariableTypes (LocalDependentVariable a)
    )
exportDynamicsUnconstrained ::
    forall sig m a b.
    HasExportDynamicsUnconstrained sig m a b =>
    StepIndex ->
    DynamicParameter b ->
    a ->
    m ()
exportDynamicsUnconstrained i t xi = do
    exportDynamicParameter i t
    exportVariableDynamics i xi
    exportDependentVariableDynamicsM i xi

type HasExportDynamicsConstrained sig m a b =
    ( Algebra sig m
    , ToField b
    , Member (Dynamics b) sig
    , Member (Lift IO) sig
    , Member (Throw SomeException) sig
    , Member Export sig
    , HasLocalDependentVariableM m a
    , ToRecord (GlobalDependentVariable a)
    , HasGlobalDependentVariableM m a
    , DefaultOrdered a
    , DefaultOrdered (GlobalDependentVariable a)
    , DefaultOrdered (LocalDependentVariable a)
    , ToLazyFields a
    , ToLazyFields (LocalDependentVariable a)
    , ToVariableTypes a
    , ToVariableTypes (LocalDependentVariable a)
    , Member (ConstrainedSystem (EqualityConstraintType a)) sig
    , ToVariableTypes (EqualityConstraintType a)
    , ToLazyFields (EqualityConstraintType a)
    , DefaultOrdered (EqualityConstraintType a)
    )
exportDynamicsConstrained ::
    forall sig m a b.
    HasExportDynamicsConstrained sig m a b =>
    StepIndex ->
    DynamicParameter b ->
    a ->
    m ()
exportDynamicsConstrained i t xi = do
    exportDynamicParameter i t
    exportVariableDynamics i xi
    l <- getLagrangeMultiplier @(EqualityConstraintType a)
    exportVariableDynamics i l
    exportDependentVariableDynamicsM i xi

mainCalcDynamics ::
    forall a b sig m.
    ( Algebra sig m
    , Additive b
    , DefaultOrdered a
    , HasDependentParameterM m a
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
    , FromLazyFields a
    , HasExportDynamicsM m a b
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
            putStrLnM "End loading cached files."
            putStrLnM $ "Last step number: " <> show i'
            putStrLnM "Deleting temp files .."
            deleteTempDynamicParametersM @b
            deleteTempVariablesDynamicsM @a interval maximumIterationNumber
            msgNewLine
            putStrLnM "-- Continue calculation from last step --"
            go interval maximumIterationNumber initialValue finalValue stepSize i' t' x' x'
        _ ->
            go interval maximumIterationNumber initialValue finalValue stepSize (StepIndex 0) initialValue x x
  where
    go (IntervalStepIndex nInterval) (MaxStepIndex iMax) (DynamicParameter initVal) (DynamicParameter finalVal) (StepSize dt) i (DynamicParameter t) xiMinus1 xi = do
        putStrLnM $ "step " ++ show i
        putStrLnM $ "parameter: " ++ show t
        when (i `mod` nInterval == StepIndex 0) $
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
                msgNewLine
                go (IntervalStepIndex nInterval) (MaxStepIndex iMax) (DynamicParameter initVal) (DynamicParameter finalVal) (StepSize dt) (succ i) (DynamicParameter (initVal .+. stimesAdd i dt)) xi xiPlus1
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
                unless (null xs || nullRecords ts) $ do
                    putStrLnM "Exporting data from cached files .."
                    exportDynamicsM i'' t'' x''
                msgNewLine
                goContinue iMax (i'', t'') x'' ts xs
            (Left str, _) -> do
                putStrLnM str
                return (i', t', x')
            (_, Left e) -> do
                putStrLnM $ displayException e
                return (i', t', x')
