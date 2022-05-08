{-# HLINT ignore "Redundant irrefutable pattern" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Formulative.Postprocess.Export.Variable.Local where

import Conduit
import Control.Carrier.Error.Either
import Control.Effect.Lift
import Control.Effect.Sum
import Control.Exception.Safe
import Control.Monad.ST.Strict (runST)
import qualified Data.ByteString.Lazy as BSL
import Data.Csv (DefaultOrdered (headerOrder), FromField (..), FromRecord (..), Name, ToField, ToRecord (toRecord))
import qualified Data.Matrix.Static.LinearAlgebra as MSL
import Data.Matrix.Static.Sparse (toTriplet)
import Data.Maybe (fromJust)
import Data.Proxy
import Data.String.Conversions
import qualified Data.Vector as V
import qualified Data.Vector.Sized as VS
import qualified Data.Vector.Storable as VST
import Formulative.Calculation.Internal.Types
import Formulative.Postprocess.Export.CSV (encodeLF)
import Formulative.Postprocess.Export.Effect
import Formulative.Postprocess.Export.IO (ensureDirOutputM)
import Formulative.Postprocess.Export.Types
import Formulative.Postprocess.Export.Variable.Class
import GHC.Generics
import GHC.Natural
import GHC.TypeNats
import Path
import Path.IO

type RelFilePath = Path Rel File
type RelFilePaths = V.Vector (Path Rel File)

class ExportFieldToFile a where
    exportFieldToFile :: RelFilePath -> a -> IO ()

instance
    ( ToField a
    , VST.Storable a
    , KnownNat k1
    , KnownNat k2
    ) =>
    ExportFieldToFile (MSL.SparseMatrix k1 k2 a)
    where
    exportFieldToFile path x = runConduit $ toTriplet x .| mapC (\x -> encodeLF [x]) .| mapM_C (BSL.appendFile (toFilePath path))

instance (ToField a) => ExportFieldToFile (MyNum a) where
    exportFieldToFile path (MyNum x) = BSL.appendFile (toFilePath path) (encodeLF [[x]])

deriving via (MyNum Double) instance ExportFieldToFile Double
deriving via (MyNum Float) instance ExportFieldToFile Float
deriving via (MyNum Int) instance ExportFieldToFile Int
deriving via (MyNum Integer) instance ExportFieldToFile Integer
deriving via (MyNum Natural) instance ExportFieldToFile Natural

instance (ToField a) => ExportFieldToFile (V.Vector a) where
    exportFieldToFile path x = BSL.appendFile (toFilePath path) (encodeLF [x])
instance (ToField a) => ExportFieldToFile (VS.Vector n a) where
    exportFieldToFile path x = exportFieldToFile path (VS.fromSized x)

nameToFilePathM name = do
    parseKey <- liftEither $ parseRelFile (convertString @Name name)
    -- TODO: VTUに対応
    fileName <- liftEither $ replaceExtension ".csv" parseKey
    OutputDir parentDir <- askOutputDir
    let filePath = parentDir </> fileName
    return filePath

-------------------------

class ExportRecordToFiles a where
    exportRecordToFilesStatics :: RelFilePaths -> a -> IO ()
    default exportRecordToFilesStatics :: (Generic a, GAppendRecordToFiles (Rep a)) => RelFilePaths -> a -> IO ()
    exportRecordToFilesStatics h x = gexportRecordToFilesStatics h (from x)

    exportRecordToFilesDynamics :: StepIndex -> RelFilePaths -> a -> IO ()
    default exportRecordToFilesDynamics :: (Generic a, GAppendRecordToFiles (Rep a)) => StepIndex -> RelFilePaths -> a -> IO ()
    exportRecordToFilesDynamics i h x = gexportRecordToFilesDynamics i h (from x)

class GAppendRecordToFiles f where
    gexportRecordToFilesStatics :: RelFilePaths -> f a -> IO ()
    gexportRecordToFilesDynamics :: StepIndex -> RelFilePaths -> f a -> IO ()

instance GAppendRecordToFiles U1 where
    gexportRecordToFilesStatics h _ = return ()
    gexportRecordToFilesDynamics i h _ = return ()

instance (ExportFieldToFile a, ToVariableType a) => GAppendRecordToFiles (K1 i a) where
    gexportRecordToFilesStatics h (K1 a) = exportFieldToFile (V.head h) a
    gexportRecordToFilesDynamics i h (K1 a) = exportFieldToFileDynamics i (V.head h) a

instance GAppendRecordToFiles f => GAppendRecordToFiles (M1 i c f) where
    gexportRecordToFilesStatics h (M1 a) = gexportRecordToFilesStatics h a
    gexportRecordToFilesDynamics i h (M1 a) = gexportRecordToFilesDynamics i h a

instance (GAppendRecordToFiles a, GAppendRecordToFiles b) => GAppendRecordToFiles (a :*: b) where
    gexportRecordToFilesStatics h (a :*: b) = gexportRecordToFilesStatics (V.init h) a >> gexportRecordToFilesStatics (V.tail h) b
    gexportRecordToFilesDynamics i h (a :*: b) = gexportRecordToFilesDynamics i (V.init h) a >> gexportRecordToFilesDynamics i (V.tail h) b

instance ExportRecordToFiles ()

namesToFilePaths names = sequenceA $ V.map nameToFilePathM names

getFilePathsM ::
    forall a sig m.
    ( DefaultOrdered a
    , Algebra sig m
    , Member (Throw SomeException) sig
    , Member Export sig
    ) =>
    a ->
    m (V.Vector (Path Rel File))
getFilePathsM ~_ =
    let names = headerOrder (undefined :: a)
     in namesToFilePaths names

exportRecordToFilesStaticsM x = do
    paths <- getFilePathsM x
    ensureDirOutputM
    sendIO $ exportRecordToFilesStatics paths x

concatPathDynamics (StepIndex i) path = do
    parentDirStep <- parseRelDir $ "series/step" <> show i
    let parentDir = parent path
        fileName = filename path
    return $ parentDir </> parentDirStep </> fileName

exportFieldToFileDynamics ::
    forall a.
    (ToVariableType a, ExportFieldToFile a) =>
    StepIndex ->
    RelFilePath ->
    a ->
    IO ()
exportFieldToFileDynamics i path x = case toVariableType (Proxy @a) of
    ParticleType -> exportFieldToFile path x
    FieldType -> do
        p <- concatPathDynamics i path
        ensureDir (parent p)
        exportFieldToFile p x

exportRecordToFilesDynamicsM i x = do
    paths <- getFilePathsM x
    sendIO $ exportRecordToFilesDynamics i paths x
