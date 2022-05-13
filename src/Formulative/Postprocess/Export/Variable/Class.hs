{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Postprocess.Export.Variable.Class where

import qualified Data.Matrix.Static.LinearAlgebra.Types as MSL
import Data.Proxy
import qualified Data.Vector as V
import qualified Data.Vector.Sized as VS
import Dhall
import Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Types (DECrepresentationMatrix)
import Formulative.Calculation.Internal.List
import Formulative.Calculation.Internal.Types
import GHC.Generics
import qualified Numeric.LinearAlgebra.Data as H

data VariableType = ParticleType | FieldType
    deriving stock (Show, Eq, Generic)

class ToVariableType a where
    toVariableType :: Proxy a -> VariableType

instance (Num a) => ToVariableType (MyNum a) where
    toVariableType _ = ParticleType
instance ToVariableType () where
    toVariableType _ = ParticleType
deriving via (MyNum Double) instance ToVariableType Double
deriving via (MyNum Float) instance ToVariableType Float
deriving via (MyNum Int) instance ToVariableType Int
deriving via (MyNum Integer) instance ToVariableType Integer
deriving via (MyNum Natural) instance ToVariableType Natural

instance (Num a) => ToVariableType (MyApplicative f a) where
    toVariableType _ = ParticleType
deriving via (MyApplicative Vector a) instance (Num a) => ToVariableType (Vector a)
deriving via (MyApplicative H.Vector a) instance (Num a) => ToVariableType (H.Vector a)
deriving via (MyApplicative (VS.Vector n) a) instance (Num a) => ToVariableType (VS.Vector n a)

instance ToVariableType (MyMatrix a) where
    toVariableType _ = FieldType
deriving via (MyMatrix (DECrepresentationMatrix n l c1 k1 c2 k2 a)) instance ToVariableType (DECrepresentationMatrix n l c1 k1 c2 k2 a)
deriving via (MyMatrix (MSL.SparseMatrix p1 p2 a)) instance ToVariableType (MSL.SparseMatrix p1 p2 a)
deriving via (MyMatrix (MSL.Matrix p1 p2 a)) instance ToVariableType (MSL.Matrix p1 p2 a)
deriving via (MyMatrix (H.Matrix a)) instance ToVariableType (H.Matrix a)

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

-- instance ToVariableTypes ()
