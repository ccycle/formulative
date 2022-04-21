{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RebindableSyntax #-}

module Formulative.Postprocess.Export.Dynamics where

import Control.Carrier.Lift
import Control.Effect.Error
import Control.Effect.Sum
import Control.Exception.Safe
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import Data.Csv hiding (Field)
import Data.String
import Data.String.Conversions
import qualified Data.Vector as V
import Dhall
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.DifferentialEquation.Dynamics.Effect
import Formulative.Calculation.DifferentialEquation.Types
import Formulative.Calculation.Internal.Class
import Formulative.Calculation.Internal.IfThenElse
import Formulative.Calculation.Internal.Variable.Effect
import Formulative.Postprocess.Export.Class
import Formulative.Postprocess.Export.Effect
import Formulative.Postprocess.Export.Statics
import Formulative.Postprocess.Export.ToRecords
import Formulative.Postprocess.Export.Types
import Formulative.Preprocess.SettingFile.Effect
import Path
import Path.IO
import Prelude hiding (fromInteger)

exportParameter ::
    forall b sig m.
    ( Algebra sig m
    , ToField b
    , Member (Lift IO) sig
    , Member (Throw SomeException) sig
    , Member Export sig
    , Member (Dynamics b) sig
    ) =>
    IndexOfStep ->
    Parameter b ->
    m ()
exportParameter (IndexOfStep i) (Parameter t) = do
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

exportVariableDynamic ::
    ( Algebra sig m
    , Member (Throw SomeException) sig
    , Member (Lift IO) sig
    , Member Export sig
    , ToRecords a
    , DefaultOrdered a
    ) =>
    IndexOfStep ->
    a ->
    m ()
exportVariableDynamic (IndexOfStep i) x = do
    eType <- askEquationType
    case eType of
        ODE -> exportVariableDynamicN0 x
        PDE -> do
            (OutputDir parentDir) <- askOutputDir
            parentDir' <- liftEither $ parseRelDir $ "series/step" <> show i
            localOutputDir (\(OutputDir path) -> OutputDir $ path </> parentDir') $ exportVariableStatic x
  where
    -- n=0
    -- ファイルごとに書き出し,各時刻で追記
    exportVariableDynamicN0 x = do
        OutputDir parentDir <- askOutputDir
        ensureDirOutputM
        let x' = V.zip (headerOrder x) (toRecords x)
        forM_ x' $ \(key, str) -> do
            parseKey <- liftEither $ parseRelFile (convertString key)
            -- TODO: VTUに対応
            fileName <- liftEither $ replaceExtension ".csv" parseKey
            let filePath = parentDir </> fileName
            sendIO $ BSL.appendFile (toFilePath filePath) (encode [str])

exportDependentVariableLocalDynamic i x = do
    x' <- dependentVariableLocalM x
    exportVariableDynamic i x'

mainCalculationDynamic ::
    forall a b sig m.
    ( Algebra sig m
    , Additive b
    , HasInitialConditionM m a
    , HasDependentVariableGlobalM m a
    , HasDependentVariableLocalM m a
    , HasUpdateM m a
    , Member (Dynamics b) sig
    , Member (Variable a) sig
    , Member (Lift IO) sig
    , Member (Throw SomeException) sig
    , Member Export sig
    , Member SettingFile sig
    , Ord b
    , Show b
    , ToField b
    , Rng b
    , ToRecords a
    , ToRecord (DependentVariableGlobalType a)
    , ToNamedRecord (DependentVariableGlobalType a)
    , DefaultOrdered a
    , DefaultOrdered (DependentVariableGlobalType a)
    , HasDependentParameterM m a
    , ToDhall (DependentParameterType a)
    , ToRecords (DependentVariableLocalType a)
    , DefaultOrdered (DependentVariableLocalType a)
    ) =>
    m ()
mainCalculationDynamic = do
    preprocessM @m @a
    x <- getInitialConditionM @m @a
    DynamicParameterSetting{..} <- askDynamicParameterSetting @b
    msgStart
    go 0 interval maximumIterationNumber initialValue finalValue (StepSize stepSize) x x
    msgOutputDir
  where
    go i nInterval iMax t finalVal (StepSize dt) xiMinus1 xi =
        if iMax <= i || (2 .*. finalVal <= (2 .*. t .+. dt) && 2 .*. t <= (2 .*. finalVal .+. dt)) || finalVal < t
            then do
                putStrLnM $ "step " ++ show i
                putStrLnM $ "parameter: " ++ show t
                putStrLnM "Exporting data.."
                exportParameter (IndexOfStep i) (Parameter t)
                exportVariableDynamic (IndexOfStep i) xi
                exportDependentVariableLocalDynamic (IndexOfStep i) xi
                exportDependentVariableGlobal xi
                msgDone
                msgEnd
            else do
                putStrLnM $ "step " ++ show i
                putStrLnM $ "parameter: " ++ show t
                when (i `mod` nInterval == 0) $ do
                    putStrLnM "Exporting data.."
                    putVariableOld xiMinus1
                    exportParameter (IndexOfStep i) (Parameter t)
                    exportVariableDynamic (IndexOfStep i) xi
                    exportDependentVariableLocalDynamic (IndexOfStep i) xi
                    exportDependentVariableGlobal xi
                    msgDone
                putStrLnM "Updating variable.."
                putVariableOld xi
                xiPlus1 <- updateM xi
                putVariable xiPlus1
                msgDone
                go (succ i) nInterval iMax (t .+. dt) finalVal (StepSize dt) xi xiPlus1
