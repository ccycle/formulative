{-# LANGUAGE AllowAmbiguousTypes #-}

module Formulative.Preprocess.ReadFile where

import Control.Applicative
import Control.Effect.Lift
import Control.Effect.Sum
import Control.Effect.Throw
import Control.Exception.Safe
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Csv
import qualified Data.Csv.Streaming as Streaming
import Data.Foldable (sequenceA_)
import Data.Maybe (fromJust)
import Data.String.Conversions (ConvertibleStrings (convertString))
import Data.Typeable
import qualified Data.Vector as V
import qualified Data.Vector.Sized as VS
import Formulative.Calculation.DifferentialEquation.Dynamics.Effect
import Formulative.Calculation.DifferentialEquation.Types
import Formulative.Calculation.Internal.Types
import Formulative.Postprocess.Export.Effect
import Formulative.Postprocess.Export.IO
import Formulative.Postprocess.Export.Types
import Formulative.Postprocess.Export.Variable.Class
import Formulative.Postprocess.Export.Variable.Local (RelFilePath, RelFilePaths, concatPathDynamics, getFilePathsM, namesToFilePaths)
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

class FromLazyByteString a where
    parseBSLazy :: LazyField -> Either ReadRecordException a
instance (FromField a, Typeable a) => FromLazyByteString (MyNum a) where
    parseBSLazy x = do
        let y = parseField (BSL.toStrict x)
        case runParser y of
            Left _ -> Left (ParseException x (typeRep y))
            Right v -> Right (MyNum v)
deriving via (MyNum Double) instance FromLazyByteString Double
deriving via (MyNum Float) instance FromLazyByteString Float
deriving via (MyNum Int) instance FromLazyByteString Int
deriving via (MyNum Integer) instance FromLazyByteString Integer
deriving via (MyNum Natural) instance FromLazyByteString Natural
deriving newtype instance FromLazyByteString StepIndex
deriving newtype instance FromField StepIndex
instance (FromRecord (f a), Typeable (f a)) => FromLazyByteString (MyApplicative f a) where
    parseBSLazy x = do
        let y = V.head <$> decode NoHeader x
        case y of
            Left _ -> Left (ParseException x (typeRep y))
            Right v -> Right (MyApplicative v)
deriving via (MyApplicative V.Vector a) instance (FromField a, Typeable a) => FromLazyByteString (V.Vector a)

-- TODO: fromJustを使わない実装に直す
instance (ToField a, KnownNat n, FromField a) => FromRecord (VS.Vector n a) where
    parseRecord x = fromJust . VS.toSized <$> parseRecord x

deriving via (MyApplicative (VS.Vector n) a) instance (KnownNat n, ToField a, FromField a, Typeable a) => FromLazyByteString (VS.Vector n a)

type LazyField = BSL.ByteString
type LazyFields = V.Vector LazyField

class FromLazyFields a where
    parseLazyFields :: LazyFields -> Either ReadRecordException a
    default parseLazyFields :: (Generic a, GFromLazyFields (Rep a)) => LazyFields -> Either ReadRecordException a
    parseLazyFields x = to <$> gparseLazyFields x

    getLazyRecordsDynamics :: IntervalStepIndex -> MaxStepIndex -> RelFilePaths -> IO [Either ReadRecordException a]
    default getLazyRecordsDynamics :: (Generic a, GFromLazyFields (Rep a)) => IntervalStepIndex -> MaxStepIndex -> RelFilePaths -> IO [Either ReadRecordException a]
    getLazyRecordsDynamics i imax path = fmap (fmap (fmap to)) (ggetLazyRecordsDynamics i imax path)

class GFromLazyFields f where
    gparseLazyFields :: LazyFields -> Either ReadRecordException (f a)
    ggetLazyRecordsDynamics :: IntervalStepIndex -> MaxStepIndex -> RelFilePaths -> IO [Either ReadRecordException (f a)]

instance (FromLazyByteString a, ToVariableType a) => GFromLazyFields (K1 i a) where
    gparseLazyFields v = fmap K1 (parseBSLazy (V.head v))
    ggetLazyRecordsDynamics i imax v = case toVariableType (Proxy @a) of
        ParticleType -> fmap (fmap (fmap K1)) (decodeParticleTypeDynamics (V.head v))
        FieldType -> fmap (fmap (fmap K1)) (decodeFieldTypeDynamics i imax (V.head v))

instance GFromLazyFields f => GFromLazyFields (M1 i c f) where
    gparseLazyFields v = fmap M1 (gparseLazyFields v)
    ggetLazyRecordsDynamics i imax v = fmap (fmap (fmap M1)) (ggetLazyRecordsDynamics i imax v)

instance (GFromLazyFields a, GFromLazyFields b) => GFromLazyFields (a :*: b) where
    gparseLazyFields v = liftA2 (:*:) (gparseLazyFields (V.init v)) (gparseLazyFields (V.tail v))
    ggetLazyRecordsDynamics i imax v = liftA2 (zipWith (liftM2 (:*:))) (ggetLazyRecordsDynamics i imax (V.init v)) (ggetLazyRecordsDynamics i imax (V.tail v))

decodeParticleTypeDynamics :: forall a. FromLazyByteString a => RelFilePath -> IO [Either ReadRecordException a]
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

decodeFieldTypeDynamics :: forall a. FromLazyByteString a => IntervalStepIndex -> MaxStepIndex -> RelFilePath -> IO [Either ReadRecordException a]
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
    paths <- getFilePathsM (undefined :: a)
    sendIO $ getLazyRecordsDynamics i imax paths

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
    paths <- getFilePathsM (undefined :: a)
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
            putStrLnM $ concat ["File ", show filePath, " does not exist."]
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