{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Postprocess.Export.Variable.Class where

import Data.Proxy
import qualified Data.Vector as V
import qualified Data.Vector.Sized as VS
import Dhall
import Formulative.Calculation.Internal.List
import Formulative.Calculation.Internal.Types
import GHC.Generics

data VariableType = ParticleType | FieldType
    deriving stock (Show, Eq, Generic)

class ToVariableType a where
    toVariableType :: Proxy a -> VariableType

instance (Num a) => ToVariableType (MyNumeric a) where
    toVariableType _ = ParticleType
instance ToVariableType () where
    toVariableType _ = ParticleType
deriving via (MyNumeric Double) instance ToVariableType Double
deriving via (MyNumeric Float) instance ToVariableType Float
deriving via (MyNumeric Int) instance ToVariableType Int
deriving via (MyNumeric Integer) instance ToVariableType Integer
deriving via (MyNumeric Natural) instance ToVariableType Natural

instance (Num a) => ToVariableType (MyApplicative f a) where
    toVariableType _ = ParticleType
deriving via (MyApplicative Vector a) instance (Num a) => ToVariableType (Vector a)
deriving via (MyApplicative (VS.Vector n) a) instance (Num a) => ToVariableType (VS.Vector n a)

instance ToVariableType (MyMatrix a) where
    toVariableType _ = FieldType

----------------------------------------------------------------

type VariableTypes = [VariableType]

class ToVariableTypes a where
    toVariableTypes :: Proxy a -> VariableTypes
    default toVariableTypes :: (Generic a, GToVariableTypes (Rep a)) => Proxy a -> VariableTypes
    toVariableTypes x = gtoVariableTypes (from <$> x)

class GToVariableTypes f where
    gtoVariableTypes :: Proxy (f a) -> VariableTypes

instance GToVariableTypes U1 where
    gtoVariableTypes _ = []

instance (ToVariableType a) => GToVariableTypes (K1 i a) where
    gtoVariableTypes _ = singleton (toVariableType (Proxy @a))

instance (GToVariableTypes f) => GToVariableTypes (M1 i c f) where
    gtoVariableTypes (Proxy :: Proxy (M1 i c f a)) = gtoVariableTypes (Proxy :: Proxy (f a))

instance (GToVariableTypes a, GToVariableTypes b) => GToVariableTypes (a :*: b) where
    gtoVariableTypes (Proxy :: Proxy ((a :*: b) a1)) = gtoVariableTypes (Proxy :: Proxy (a a1)) ++ gtoVariableTypes (Proxy :: Proxy (b a1))

instance ToVariableTypes ()
