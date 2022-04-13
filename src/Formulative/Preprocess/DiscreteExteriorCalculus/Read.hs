{-# LANGUAGE DeriveAnyClass #-}

module Formulative.Preprocess.DiscreteExteriorCalculus.Read where

import Control.Applicative
import Control.Exception.Safe
import qualified Data.ByteString.Lazy as BSL
import Data.Csv
import Data.Hashable
import Data.Typeable
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Dhall

-- import Formulative.Calculation.DiscreteExteriorCalculus.Geometry
-- import Formulative.Calculation.DiscreteExteriorCalculus.Homology

import Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Types
import Formulative.Calculation.DiscreteExteriorCalculus.Geometry.Types
import Formulative.Calculation.DiscreteExteriorCalculus.Homology.Types
import Formulative.Preprocess.DefaultValue
import Path
import Refined

-- TODO: パスからファイルの拡張子の判定を行う関数を作成
data MeshPath
  = NoData
  | VTUPath {vtuPath :: FilePath}
  | GmshPath {mshPath :: FilePath}
  | CSVDataPath {pointData :: FilePath, connectivity :: FilePath, boundaryNameList :: FilePath}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromDhall, ToDhall, Hashable)
instance HasDefaultValue MeshPath where
  defaultValue = NoData

data GeometrySetting = GeometrySetting
  { meshPath :: MeshPath
  , metricSignature :: MetricSignature
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromDhall, ToDhall, Hashable, HasDefaultValue)

checkMeshDataType NoData = return NoData
checkMeshDataType (VTUPath path) = do
  path' <- parseRelFile path
  ext <- fileExtension path'
  if ".vtu" == ext then return (VTUPath path) else throw (FileExtensionException ext (VTUPath path))
checkMeshDataType (GmshPath path) = do
  path' <- parseRelFile path
  ext <- fileExtension path'
  if ".vtu" == ext then return (GmshPath path) else throw (FileExtensionException ext (GmshPath path))
checkMeshDataType (CSVDataPath path1 path2 path3) = do
  path1' <- parseRelFile path1
  path2' <- parseRelFile path2
  path3' <- parseRelFile path3
  ext1 <- fileExtension path1'
  ext2 <- fileExtension path2'
  ext3 <- fileExtension path3'
  if (".csv" == ext1) && (".csv" == ext2) && (".csv" == ext3) then return (CSVDataPath path1 path2 path3) else throw (FileExtensionException ".csv" (CSVDataPath path1 path2 path3))

data FileExtensionException = FileExtensionException String MeshPath deriving (Show)
instance Exception FileExtensionException where
  displayException (FileExtensionException ext p) = "*** FileExtensionException: "

-- TODO: error処理を作成
readFromNoHeaderCSVunsafe csvData =
  case decode NoHeader csvData of
    Left err -> error err
    Right v -> v

readFromNoHeaderCSV csvData =
  case decode NoHeader csvData of
    Left err -> throwString err
    Right v -> return v

readMatrixComponents :: forall m a. (VU.Unbox a, FromRecord a, FromField a, MonadThrow m) => BSL.ByteString -> m (COOstorage a)
readMatrixComponents s = VU.convert <$> (readFromNoHeaderCSV s :: m (V.Vector (COO a)))

-- component
readPositions :: MonadThrow m => BSL.ByteString -> m (V.Vector (V.Vector Component))
readPositions = readFromNoHeaderCSV