module Formulative.Postprocess.Export.Variable.Local where

import Conduit
import Control.Carrier.Error.Either
import Control.Effect.Lift
import Control.Monad.ST.Strict (runST)
import qualified Data.ByteString.Lazy as BSL
import Data.Csv (DefaultOrdered (headerOrder), Name, ToField, ToRecord (toRecord), encode)
import qualified Data.Matrix.Static.LinearAlgebra as MSL
import Data.Matrix.Static.Sparse (toTriplet)
import Data.String.Conversions
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VST
import Formulative.Calculation.Internal.Types
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
    exportFieldToFile path x = runConduit $ toTriplet x .| mapC (\x -> encode [x]) .| mapM_C (BSL.appendFile (toFilePath path))

instance (ToField a) => ExportFieldToFile (MyNum a) where
    exportFieldToFile path (MyNum x) = BSL.appendFile (toFilePath path) (encode [[x]])

deriving via (MyNum Double) instance ExportFieldToFile Double
deriving via (MyNum Float) instance ExportFieldToFile Float
deriving via (MyNum Int) instance ExportFieldToFile Int
deriving via (MyNum Integer) instance ExportFieldToFile Integer
deriving via (MyNum Natural) instance ExportFieldToFile Natural

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

    exportRecordToFilesDynamics :: IndexOfStep -> RelFilePaths -> a -> IO ()
    default exportRecordToFilesDynamics :: (Generic a, GAppendRecordToFiles (Rep a)) => IndexOfStep -> RelFilePaths -> a -> IO ()
    exportRecordToFilesDynamics i h x = gexportRecordToFilesDynamics i h (from x)

class GAppendRecordToFiles f where
    gexportRecordToFilesStatics :: RelFilePaths -> f a -> IO ()
    gexportRecordToFilesDynamics :: IndexOfStep -> RelFilePaths -> f a -> IO ()

instance (ExportFieldToFile a, ToVariableType a) => GAppendRecordToFiles (K1 i a) where
    gexportRecordToFilesStatics h (K1 a) = exportFieldToFile (V.head h) a
    gexportRecordToFilesDynamics i h (K1 a) = exportFieldToFileDynamics i (V.head h) a

instance GAppendRecordToFiles f => GAppendRecordToFiles (M1 i c f) where
    gexportRecordToFilesStatics h (M1 a) = gexportRecordToFilesStatics h a
    gexportRecordToFilesDynamics i h (M1 a) = gexportRecordToFilesDynamics i h a

instance (GAppendRecordToFiles a, GAppendRecordToFiles b) => GAppendRecordToFiles (a :*: b) where
    gexportRecordToFilesStatics h (a :*: b) = gexportRecordToFilesStatics (V.init h) a >> gexportRecordToFilesStatics (V.tail h) b
    gexportRecordToFilesDynamics i h (a :*: b) = gexportRecordToFilesDynamics i (V.init h) a >> gexportRecordToFilesDynamics i (V.tail h) b

namesToFilePaths names = sequenceA $ V.map nameToFilePathM names

exportRecordToFilesStaticsM x = do
    let names = headerOrder x
    paths <- namesToFilePaths names
    ensureDirOutputM
    sendIO $ exportRecordToFilesStatics paths x

exportFieldToFileDynamics (IndexOfStep i) path x = case toVariableType x of
    ParticleType -> exportFieldToFile path x
    FieldType -> do
        parentDirStep <- parseRelDir $ "series/step" <> show i
        let parentDir = parent path
        let fileName = filename path
        ensureDir (parentDir </> parentDirStep)
        exportFieldToFile (parentDir </> parentDirStep </> fileName) x

exportRecordToFilesDynamicsM i x = do
    let names = headerOrder x
    paths <- namesToFilePaths names
    sendIO $ exportRecordToFilesDynamics i paths x
