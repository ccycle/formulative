module Formulative.Postprocess.Export.WriteFile where

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

class AppendFieldToFile a where
    appendFieldToFile :: RelFilePath -> a -> IO ()

instance
    ( ToField a
    , VST.Storable a
    , KnownNat k1
    , KnownNat k2
    ) =>
    AppendFieldToFile (MSL.SparseMatrix k1 k2 a)
    where
    appendFieldToFile path x = runConduit $ toTriplet x .| mapC (\x -> encode [x]) .| mapM_C (BSL.appendFile (toFilePath path))

instance (ToField a) => AppendFieldToFile (MyNum a) where
    appendFieldToFile path (MyNum x) = BSL.appendFile (toFilePath path) (encode [[x]])

deriving via (MyNum Double) instance AppendFieldToFile Double
deriving via (MyNum Float) instance AppendFieldToFile Float
deriving via (MyNum Int) instance AppendFieldToFile Int
deriving via (MyNum Integer) instance AppendFieldToFile Integer
deriving via (MyNum Natural) instance AppendFieldToFile Natural

nameToFilePathM name = do
    parseKey <- liftEither $ parseRelFile (convertString @Name name)
    -- TODO: VTUに対応
    fileName <- liftEither $ replaceExtension ".csv" parseKey
    OutputDir parentDir <- askOutputDir
    let filePath = parentDir </> fileName
    return filePath

appendFileFromNameM name x = do
    fileName <- nameToFilePathM name
    sendIO $ appendFieldToFile fileName x

-------------------------

class AppendRecordToFiles a where
    appendRecordToFiles :: RelFilePaths -> a -> IO ()
    default appendRecordToFiles :: (Generic a, GAppendRecordToFiles (Rep a)) => RelFilePaths -> a -> IO ()
    appendRecordToFiles h x = gappendRecordToFiles h (from x)

    appendRecordToFilesDynamics :: IndexOfStep -> RelFilePaths -> a -> IO ()
    default appendRecordToFilesDynamics :: (Generic a, GAppendRecordToFiles (Rep a)) => IndexOfStep -> RelFilePaths -> a -> IO ()
    appendRecordToFilesDynamics i h x = gappendRecordToFilesDynamics i h (from x)

class GAppendRecordToFiles f where
    gappendRecordToFiles :: RelFilePaths -> f a -> IO ()
    gappendRecordToFilesDynamics :: IndexOfStep -> RelFilePaths -> f a -> IO ()

instance (AppendFieldToFile a, ToVariableType a) => GAppendRecordToFiles (K1 i a) where
    gappendRecordToFiles h (K1 a) = appendFieldToFile (V.head h) a
    gappendRecordToFilesDynamics i h (K1 a) = appendFileDynamics i (V.head h) a

instance GAppendRecordToFiles f => GAppendRecordToFiles (M1 i c f) where
    gappendRecordToFiles h (M1 a) = gappendRecordToFiles h a
    gappendRecordToFilesDynamics i h (M1 a) = gappendRecordToFilesDynamics i h a

instance (GAppendRecordToFiles a, GAppendRecordToFiles b) => GAppendRecordToFiles (a :*: b) where
    gappendRecordToFiles h (a :*: b) = gappendRecordToFiles (V.init h) a >> gappendRecordToFiles (V.tail h) b
    gappendRecordToFilesDynamics i h (a :*: b) = gappendRecordToFilesDynamics i (V.init h) a >> gappendRecordToFilesDynamics i (V.tail h) b

namesToFilePaths names = sequenceA $ V.map nameToFilePathM names

appendFilesM x = do
    let names = headerOrder x
    paths <- namesToFilePaths names
    ensureDirOutputM
    sendIO $ appendRecordToFiles paths x

appendFileDynamics (IndexOfStep i) path x = case toVariableType x of
    ParticleType -> appendFieldToFile path x
    FieldType -> do
        parentDirStep <- parseRelDir $ "series/step" <> show i
        let parentDir = parent path
        let fileName = filename path
        ensureDir (parentDir </> parentDirStep)
        appendFieldToFile (parentDir </> parentDirStep </> fileName) x

appendFilesDynamicsM i x = do
    let names = headerOrder x
    paths <- namesToFilePaths names
    sendIO $ appendRecordToFilesDynamics i paths x

-- Record for sized vector
-- instance (ToField a) => ToRecord (VS.Vector n a) where
--     toRecord x = toRecord (VS.fromSized x)
-- instance AppendRecordToFiles () where
--     appendRecordToFiles _ = V.empty

-- instance (ToField a) => ToRecord (MyNum a) where
--     toRecord (MyNum x) = V.singleton $ toField x
-- deriving via (MyNum Double) instance ToRecord Double
-- deriving via (MyNum Float) instance ToRecord Float
-- deriving via (MyNum Int) instance ToRecord Int
-- deriving via (MyNum Integer) instance ToRecord Integer
-- deriving via (MyNum Natural) instance ToRecord Natural