{-# LANGUAGE AllowAmbiguousTypes #-}

module Formulative.Preprocess.ReadFile where

import Control.Applicative
import Control.Effect.Lift
import Control.Effect.Sum
import Control.Effect.Throw
import Control.Exception.Safe
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Csv
import qualified Data.Csv.Streaming as Streaming
import Data.Foldable (sequenceA_)
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HM
import Data.Maybe (fromJust)
import Data.String.Conversions (ConvertibleStrings (convertString))
import Data.Typeable
import qualified Data.Vector as V
import qualified Data.Vector.Sized as VS
import Formulative.Calculation.Coordinates.Dim2.Euclidean
import Formulative.Calculation.Coordinates.Dim3.Euclidean
import Formulative.Calculation.DifferentialEquation.Dynamics.Effect
import Formulative.Calculation.DifferentialEquation.Types
import Formulative.Calculation.Internal.Types
import Formulative.Postprocess.Export.Effect
import Formulative.Postprocess.Export.IO
import Formulative.Postprocess.Export.Types
import Formulative.Postprocess.Export.Variable.Class
import Formulative.Postprocess.Export.Variable.Local (PathRelFile, PathRelFiles, concatPathDynamics, getFilePathsM, namesToFilePaths)
import Formulative.Preprocess.Path
import GHC.Generics
import GHC.Natural
import GHC.TypeNats
import Path
import Path.IO

data ReadRecordException = ParseException BSL.ByteString TypeRep | FileDoesNotExistException FilePath
    deriving (Show, Typeable, Generic)
instance Exception ReadRecordException where
    displayException (ParseException x y) = concat ["*** ParseException: ", "\n", "input: ", convertString x, "\n", "expected type: ", show y]
    displayException (FileDoesNotExistException path) = concat ["*** FileDoesNotExistException: ", "\n", "The path ", path, " is not found."]

class FromLazyField a where
    parseBSLazy :: LazyField -> Either ReadRecordException a
instance (FromField a, Typeable a) => FromLazyField (MyNum a) where
    parseBSLazy x = do
        let y = parseField (BSL.toStrict x)
        case runParser y of
            Left _ -> Left (ParseException x (typeRep y))
            Right v -> Right (MyNum v)
deriving via (MyNum Double) instance FromLazyField Double
deriving via (MyNum Float) instance FromLazyField Float
deriving via (MyNum Int) instance FromLazyField Int
deriving via (MyNum Integer) instance FromLazyField Integer
deriving via (MyNum Natural) instance FromLazyField Natural
deriving newtype instance FromLazyField StepIndex
deriving newtype instance FromField StepIndex
instance (FromRecord (f a), Typeable (f a)) => FromLazyField (MyApplicative f a) where
    parseBSLazy x = do
        let y = V.head <$> decode NoHeader x
        case y of
            Left _ -> Left (ParseException x (typeRep y))
            Right v -> Right (MyApplicative v)
deriving via (MyApplicative V.Vector a) instance (FromField a, Typeable a) => FromLazyField (V.Vector a)
deriving via (MyApplicative EuclideanCoord2d a) instance (FromField a, Typeable a) => FromLazyField (EuclideanCoord2d a)
deriving via (MyApplicative EuclideanCoord3d a) instance (FromField a, Typeable a) => FromLazyField (EuclideanCoord3d a)

-- TODO: fromJustを使わない実装に直す
instance (ToField a, KnownNat n, FromField a) => FromRecord (VS.Vector n a) where
    parseRecord x = fromJust . VS.toSized <$> parseRecord x

deriving via (MyApplicative (VS.Vector n) a) instance (KnownNat n, ToField a, FromField a, Typeable a) => FromLazyField (VS.Vector n a)

type LazyField = BSL.ByteString

type NamedRelFilePaths = HashMap Name (Path Rel File)
type NamedLazyFields = HashMap Name LazyField

type SequentialLazyField = [LazyField]
type NamedSequentialLazyField = HashMap Name SequentialLazyField

class FromLazyFields a where
    parseLazyFields :: NamedLazyFields -> Either ReadRecordException a
    default parseLazyFields :: (Generic a, GFromLazyFields (Rep a)) => NamedLazyFields -> Either ReadRecordException a
    parseLazyFields x = to <$> gparseLazyFields x

    getLazyRecordsDynamics :: IntervalStepIndex -> MaxStepIndex -> NamedRelFilePaths -> IO [Either ReadRecordException a]
    default getLazyRecordsDynamics :: (Generic a, GFromLazyFields (Rep a)) => IntervalStepIndex -> MaxStepIndex -> NamedRelFilePaths -> IO [Either ReadRecordException a]
    getLazyRecordsDynamics i imax path = fmap (fmap (fmap to)) (ggetLazyRecordsDynamics i imax path)

class GFromLazyFields f where
    gparseLazyFields :: NamedLazyFields -> Either ReadRecordException (f a)
    ggetLazyRecordsDynamics :: IntervalStepIndex -> MaxStepIndex -> NamedRelFilePaths -> IO [Either ReadRecordException (f a)]

-- instance (FromLazyField a, ToVariableType a) => GFromLazyFields (K1 i a) where
--     gparseLazyFields v = fmap K1 (parseBSLazy (head v))
--     ggetLazyRecordsDynamics i imax v = case toVariableType (Proxy @a) of
--         ParticleType -> fmap (fmap (fmap K1)) (decodeParticleTypeDynamics (head v))
--         FieldType -> fmap (fmap (fmap K1)) (decodeFieldTypeDynamics i imax (head v))

instance (Selector s, FromLazyField a, ToVariableType a) => GFromLazyFields (M1 S s (K1 i a)) where
    gparseLazyFields v = fmap (M1 . K1) (parseBSLazy str)
      where
        name = selName (undefined :: M1 S s (K1 i a) b)
        key = convertString name
        str = v HM.! key
    ggetLazyRecordsDynamics i imax v = case toVariableType (Proxy @a) of
        ParticleType -> fmap (fmap (fmap (M1 . K1))) (decodeParticleTypeDynamics path)
        FieldType -> fmap (fmap (fmap (M1 . K1))) (decodeFieldTypeDynamics i imax path)
      where
        name = selName (undefined :: M1 S s (K1 i a) b)
        key = convertString name
        path = v HM.! key

instance (GFromLazyFields f) => GFromLazyFields (M1 D c f) where
    gparseLazyFields v = fmap M1 (gparseLazyFields v)
    ggetLazyRecordsDynamics i imax v = fmap (fmap (fmap M1)) (ggetLazyRecordsDynamics i imax v)
instance (GFromLazyFields f) => GFromLazyFields (M1 C c f) where
    gparseLazyFields v = fmap M1 (gparseLazyFields v)
    ggetLazyRecordsDynamics i imax v = fmap (fmap (fmap M1)) (ggetLazyRecordsDynamics i imax v)

instance (GFromLazyFields a, GFromLazyFields b) => GFromLazyFields (a :*: b) where
    gparseLazyFields v = liftA2 (:*:) (gparseLazyFields v) (gparseLazyFields v)
    ggetLazyRecordsDynamics i imax v = liftA2 (zipWith (liftM2 (:*:))) (ggetLazyRecordsDynamics i imax v) (ggetLazyRecordsDynamics i imax v)

decodeParticleTypeDynamics :: forall a. FromLazyField a => PathRelFile -> IO [Either ReadRecordException a]
decodeParticleTypeDynamics path = do
    flag <- doesFileExist path
    if flag
        then do
            -- let y = toFilePath path
            z <- readlines path
            return $ map parseBSLazy z
        else return [Left (FileDoesNotExistException (toFilePath path))]
  where
    readlines path = do
        x <- renameAndReadTempFiles path
        return $ BSL8.lines x

decodeFieldTypeDynamics :: forall a. FromLazyField a => IntervalStepIndex -> MaxStepIndex -> PathRelFile -> IO [Either ReadRecordException a]
decodeFieldTypeDynamics (IntervalStepIndex i) (MaxStepIndex imax) path = do
    when (i == 0) $ error "Condition (1 =< IntervalStepIndex) do not satisfy"
    let is = [0, i .. imax]
    pathList <- traverse (`concatPathDynamics` path) is
    strList <- traverse renameAndReadTempFiles pathList
    let xList = map parseBSLazy strList
    return xList

getLazyRecordsDynamicsM ::
    forall a sig m.
    ( Algebra sig m
    , DefaultOrdered a
    , Member (Throw SomeException) sig
    , Member Export sig
    , Member (Lift IO) sig
    , FromLazyFields a
    ) =>
    IntervalStepIndex ->
    MaxStepIndex ->
    m [Either ReadRecordException a]
getLazyRecordsDynamicsM i imax = do
    paths <- getFilePathsM (Proxy @a)
    let h = V.toList (headerOrder (undefined :: a)); pathsMap = HM.fromList (zip h paths)
    sendIO $ getLazyRecordsDynamics i imax pathsMap

deleteTempVariablesDynamicsM ::
    forall a sig m.
    ( Algebra sig m
    , DefaultOrdered a
    , Member (Throw SomeException) sig
    , Member Export sig
    , Member (Lift IO) sig
    , FromLazyFields a
    ) =>
    IntervalStepIndex ->
    MaxStepIndex ->
    m ()
deleteTempVariablesDynamicsM i imax = do
    paths <- getFilePathsM (Proxy @a)
    sendIO $ deleteTempFiles paths

decodeDynamicParametersM ::
    forall a sig m.
    ( Algebra sig m
    , Member (Dynamics a) sig
    , Member (Throw SomeException) sig
    , Member Export sig
    , Member (Lift IO) sig
    , FromField a
    ) =>
    m (Streaming.Records (StepIndex, DynamicParameter a))
decodeDynamicParametersM = do
    DynamicsSetting{..} <- askDynamicsSetting @a
    filePath <- askDynamicParameterPath @a
    flag <- sendIO $ doesFileExist filePath
    if flag
        then do
            str <- sendIO $ renameAndReadTempFiles filePath
            return $ Streaming.decode @(StepIndex, DynamicParameter a) HasHeader str
        else do
            msgLoggerM $ concat ["File ", show filePath, " does not exist."]
            return (Streaming.Nil Nothing "")

nullRecords (Streaming.Nil _ _) = True
nullRecords _ = False

deleteTempDynamicParametersM ::
    forall a sig m.
    ( Algebra sig m
    , Member (Dynamics a) sig
    , Member (Throw SomeException) sig
    , Member Export sig
    , Member (Lift IO) sig
    , FromField a
    ) =>
    m ()
deleteTempDynamicParametersM = do
    filePath <- askDynamicParameterPath @a
    sendIO $ deleteTempFile filePath