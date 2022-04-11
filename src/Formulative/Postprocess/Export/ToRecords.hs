{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Postprocess.Export.ToRecords where

import qualified Data.ByteString as B
import Data.Csv
import qualified Data.Vector as V
import qualified Data.Vector.Sized as VS
import Formulative.Calculation.Internal.Types
import GHC.Generics

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
    toRecord (MkMyNum x) = V.singleton $ toField x
deriving via (MyNum Double) instance ToRecord Double
deriving via (MyNum Float) instance ToRecord Float
deriving via (MyNum Int) instance ToRecord Int