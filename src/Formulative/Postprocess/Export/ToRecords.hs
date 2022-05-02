{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Postprocess.Export.ToRecords where

import Conduit
import Control.Monad.ST.Strict (runST)
import qualified Data.ByteString as B
import Data.Conduit
import Data.Csv
import qualified Data.Matrix.Static.LinearAlgebra as MSL
import Data.Matrix.Static.Sparse (toTriplet)
import qualified Data.Vector as V
import qualified Data.Vector.Sized as VS
import qualified Data.Vector.Storable as VST
import Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Types
import Formulative.Calculation.Internal.Types
import GHC.Generics
import GHC.Natural
import GHC.TypeNats

type Records = V.Vector Record

class ToRecords a where
    -- | Convert a value to a record.
    toRecords :: a -> Records
    default toRecords :: (Generic a, GToRecords (Rep a) Record) => a -> Records
    toRecords x = gtoRecords (from x)

class GToRecords a f where
    gtoRecords :: a p -> V.Vector f

instance ToRecord a => GToRecords (K1 i a) Record where
    gtoRecords (K1 a) = V.fromList [toRecord a]

instance GToRecords a f => GToRecords (M1 i c a) f where
    gtoRecords (M1 a) = gtoRecords a

instance (GToRecords a f, GToRecords b f) => GToRecords (a :*: b) f where
    gtoRecords (a :*: b) = gtoRecords a <> gtoRecords b

-- Record for sized vector
instance (ToField a) => ToRecord (VS.Vector n a) where
    toRecord x = toRecord (VS.fromSized x)
instance ToRecords () where
    toRecords _ = V.empty

instance (ToField a) => ToRecord (MyNum a) where
    toRecord (MyNum x) = V.singleton $ toField x
deriving via (MyNum Double) instance ToRecord Double
deriving via (MyNum Float) instance ToRecord Float
deriving via (MyNum Int) instance ToRecord Int
deriving via (MyNum Integer) instance ToRecord Integer
deriving via (MyNum Natural) instance ToRecord Natural

-- TODO: Complex Numberの場合の導出を検討

instance
    ( ToField a
    , VST.Storable a
    , KnownNat k1
    , KnownNat k2
    ) =>
    ToRecord (MSL.SparseMatrix k1 k2 a)
    where
    toRecord x = runST $ runConduit $ toTriplet x .| mapC toRecord .| foldlC (<>) V.empty

-- TODO: proofを追加 型制約を(KnownNat k1, KnownNat k2)にする

instance
    ( ToField a
    , VST.Storable a
    , KnownNat (ToMatSize n l c1 k1)
    , KnownNat (ToMatSize n l c2 k2)
    ) =>
    ToRecord (DECrepresentationMatrix n l c1 k1 c2 k2 a)
    where
    toRecord (DECrepresentationMatrix x) = toRecord x