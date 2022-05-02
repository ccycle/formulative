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
import Formulative.Postprocess.Export.Effect
import Formulative.Postprocess.Export.IO
import Formulative.Postprocess.Export.Statics
import Formulative.Postprocess.Export.ToRecords
import Formulative.Postprocess.Export.Types
import Formulative.Postprocess.Export.Variable.Class
import Formulative.Postprocess.Export.Variable.Global
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

exportVariableDynamics (IndexOfStep i) (t, name, x) = do
    let str = toRecord x
    parseKey <- liftEither $ parseRelFile (convertString name)
    -- TODO: VTUに対応
    fileName <- liftEither $ replaceExtension ".csv" parseKey
    OutputDir parentDir <- askOutputDir
    let filePath = parentDir </> fileName
    case t of
        ParticleType -> sendIO $ BSL.appendFile (toFilePath filePath) (encode [str])
        FieldType -> do
            parentDir' <- liftEither $ parseRelDir $ "series/step" <> show i
            localOutputDir (\(OutputDir path) -> OutputDir $ path </> parentDir') $
                sendIO $ BSL.writeFile (toFilePath filePath) (encode [str])

exportVariablesDynamics ::
    ( Algebra sig m
    , Member (Throw SomeException) sig
    , Member (Lift IO) sig
    , Member Export sig
    , ToRecords a
    , DefaultOrdered a
    , ToVariableTypes a
    ) =>
    IndexOfStep ->
    a ->
    m ()
exportVariablesDynamics (IndexOfStep i) x = do
    let ts = toVariableTypes x
    let names = headerOrder x
    let xs = toRecords x
    let var = V.zip3 ts names xs
    forM_ var (exportVariableDynamics (IndexOfStep i))

--   where
-- eType <- askEquationType
-- case eType of
--     ODE -> exportVariableDynamicN0 x
--     PDE -> do
--         (OutputDir parentDir) <- askOutputDir
--         parentDir' <- liftEither $ parseRelDir $ "series/step" <> show i
--         localOutputDir (\(OutputDir path) -> OutputDir $ path </> parentDir') $ exportVariableStatic x

-- n=0
-- ファイルごとに書き出し,各時刻で追記
-- exportVariableDynamicN0 x = do
--     OutputDir parentDir <- askOutputDir
--     ensureDirOutputM
--     let x' = V.zip (headerOrder x) (toRecords x)
--     forM_ x' $ \(name, str) -> do
--         parseKey <- liftEither $ parseRelFile (convertString name)
--         -- TODO: VTUに対応
--         fileName <- liftEither $ replaceExtension ".csv" parseKey
--         let filePath = parentDir </> fileName
--         sendIO $ BSL.appendFile (toFilePath filePath) (encode [str])

exportDependentVariablesLocalDynamics ::
    ( HasLocalDependentVariableM m a
    , Algebra sig m
    , Member (Throw SomeException) sig
    , Member (Lift IO) sig
    , Member Export sig
    , ToRecords (LocalDependentVariable a)
    , DefaultOrdered (LocalDependentVariable a)
    , ToVariableTypes (LocalDependentVariable a)
    ) =>
    IndexOfStep ->
    a ->
    m ()
exportDependentVariablesLocalDynamics i x = do
    x' <- dependentVariableLocalM x
    exportVariablesDynamics i x'

exportDynamicsM ::
    ( Algebra sig m
    , ToRecords a
    , Member Export sig
    , Member (Lift IO) sig
    , Member (Dynamics b) sig
    , Member (Throw SomeException) sig
    , HasLocalDependentVariableM m a
    , HasGlobalDependentVariableM m a
    , DefaultOrdered a
    , ToRecords (LocalDependentVariable a)
    , DefaultOrdered (LocalDependentVariable a)
    , DefaultOrdered (GlobalDependentVariable a)
    , ToRecord (GlobalDependentVariable a)
    , ToField b
    , ToVariableTypes a
    , ToVariableTypes (LocalDependentVariable a)
    ) =>
    IndexOfStep ->
    DynamicParameter b ->
    a ->
    m ()
exportDynamicsM (IndexOfStep i) (DynamicParameter t) xi = do
    exportDynamicParameter (IndexOfStep i) (DynamicParameter t)
    exportVariablesDynamics (IndexOfStep i) xi
    exportDependentVariablesGlobal xi
    exportDependentVariablesLocalDynamics (IndexOfStep i) xi

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
    , ToRecords (LocalDependentVariable a)
    , ToRecords a
    , ToVariableTypes (LocalDependentVariable a)
    , ToVariableTypes a
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
