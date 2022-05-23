{-# HLINT ignore "Redundant irrefutable pattern" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Formulative.Postprocess.Export.Variable.Local where

import Conduit
import Control.Carrier.Error.Either
import Control.Effect.Lift
import Control.Effect.Sum
import Control.Exception.Safe
import Control.Monad
import Control.Monad.ST.Strict (runST)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy as BSL8
import Data.Csv
import Data.Maybe (fromJust)
import Data.Proxy
import Data.String.Conversions
import qualified Data.Vector as V
import qualified Data.Vector.Sized as VS
import qualified Data.Vector.Storable as VST
import Formulative.Calculation.Internal.List
import Formulative.Calculation.Internal.Types
import Formulative.Postprocess.Export.CSV (encodeLF)
import Formulative.Postprocess.Export.Effect
import Formulative.Postprocess.Export.IO (ensureDirOutputM, msgLoggerM)
import Formulative.Postprocess.Export.Types
import Formulative.Postprocess.Export.Variable.Class
import GHC.Generics
import GHC.Natural
import GHC.TypeNats
import Path
import Path.IO

type PathRelFile = Path Rel File
type PathRelFiles = [Path Rel File]

type LazyField = BSL.ByteString

class ToLazyField a where
    toLazyField :: a -> LazyField
    default toLazyField :: (ToRecord a) => a -> LazyField
    toLazyField x = encodeLF [x]

instance (ToField a) => ToLazyField (MyNum a) where
    toLazyField (MyNum x) = encodeLF [[x]]

deriving via (MyNum Double) instance ToLazyField Double
deriving via (MyNum Float) instance ToLazyField Float
deriving via (MyNum Int) instance ToLazyField Int
deriving via (MyNum Integer) instance ToLazyField Integer
deriving via (MyNum Natural) instance ToLazyField Natural

instance (ToField a) => ToLazyField (V.Vector a)
instance (ToField a) => ToLazyField (VS.Vector n a) where
    toLazyField x = toLazyField (VS.fromSized x)

nameToFilePathM name = do
    parseKey <- liftEither $ parseRelFile (convertString @Name name)
    fileName <- liftEither $ replaceExtension ".csv" parseKey
    OutputDir parentDir <- askOutputDir
    let filePath = parentDir </> fileName
    return filePath

-------------------------

type LazyFields = [BSL.ByteString]

class ToLazyFields a where
    toLazyFields :: a -> LazyFields
    default toLazyFields :: (Generic a, GToLazyFields (Rep a)) => a -> LazyFields
    toLazyFields x = gtoLazyFields (from x)

class GToLazyFields f where
    gtoLazyFields :: f a -> LazyFields

instance GToLazyFields U1 where
    gtoLazyFields _ = emptyV

instance (ToLazyField a) => GToLazyFields (K1 i a) where
    gtoLazyFields (K1 a) = singleton (toLazyField a)

instance (GToLazyFields f) => GToLazyFields (M1 i c f) where
    gtoLazyFields (M1 a) = gtoLazyFields a

instance (GToLazyFields a, GToLazyFields b) => GToLazyFields (a :*: b) where
    gtoLazyFields (a :*: b) = gtoLazyFields a <> gtoLazyFields b

instance ToLazyFields () where
    toLazyFields _ = emptyV

namesToFilePaths name = traverse nameToFilePathM name

getFilePathsM ::
    forall a sig m.
    ( DefaultOrdered a
    , Algebra sig m
    , Member (Throw SomeException) sig
    , Member Export sig
    ) =>
    Proxy a ->
    m PathRelFiles
getFilePathsM _ =
    let names = V.toList $ headerOrder (undefined :: a)
     in traverse nameToFilePathM names

concatPathDynamics (StepIndex i) path = do
    parentDirStep <- parseRelDir $ "series/step" <> show i
    let parentDir = parent path
        fileName = filename path
    return $ parentDir </> parentDirStep </> fileName

exportFieldToFileDynamics i vType path str = case vType of
    ParticleType ->
        BSL.appendFile (toFilePath path) str
    FieldType -> do
        p <- concatPathDynamics i path
        ensureDir (parent p)
        BSL.writeFile (toFilePath path) str

exportRecordToFilesStatics :: forall a. (ToVariableTypes a, ToLazyFields a) => PathRelFiles -> a -> IO ()
exportRecordToFilesStatics paths x = do
    let strs = toLazyFields x
    forM_ (zip paths strs) $ \(path, str) ->
        BSL.writeFile (toFilePath path) str

exportRecordToFilesDynamics :: forall a. (ToVariableTypes a, ToLazyFields a) => StepIndex -> PathRelFiles -> a -> IO ()
exportRecordToFilesDynamics i paths x = do
    let vTypes = toVariableTypes (Proxy @a)
        strs = toLazyFields x
    forM_ (zip3 vTypes paths strs) $ \(vType, path, str) ->
        exportFieldToFileDynamics i vType path str

exportRecordToFilesStaticsM ::
    forall sig m a.
    ( Algebra sig m
    , DefaultOrdered a
    , Member (Lift IO) sig
    , Member (Throw SomeException) sig
    , Member Export sig
    , ToLazyFields a
    , ToVariableTypes a
    ) =>
    a ->
    m ()
exportRecordToFilesStaticsM x = do
    paths <- getFilePathsM (Proxy @a)
    sendIO $ exportRecordToFilesStatics paths x

exportRecordToFilesDynamicsM ::
    forall sig m a.
    ( Algebra sig m
    , DefaultOrdered a
    , Member (Lift IO) sig
    , Member (Throw SomeException) sig
    , Member Export sig
    , ToLazyFields a
    , ToVariableTypes a
    ) =>
    StepIndex ->
    a ->
    m ()
exportRecordToFilesDynamicsM i x = do
    paths <- getFilePathsM (Proxy @a)
    sendIO $ exportRecordToFilesDynamics i paths x

msgExportFile x = do
    paths <- getFilePathsM x
    forM_ (map toFilePath paths) (\s -> msgLoggerM $ concat ["Exporting ", s, " .."])