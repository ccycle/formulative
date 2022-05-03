{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Postprocess.Export.Variable.Class where

import qualified Data.Matrix.Static.LinearAlgebra.Types as MSL
import qualified Data.Vector as V
import qualified Data.Vector.Sized as VS
import Dhall
import Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Types (DECrepresentationMatrix)
import Formulative.Calculation.Internal.Types
import GHC.Generics

data VariableType = ParticleType | FieldType

class ToVariableType a where
    toVariableType :: a -> VariableType

-- type VariableTypes = V.Vector VariableType

-- class ToVariableTypes a where
--     toVariableTypes :: a -> VariableTypes
--     default toVariableTypes :: (Generic a, GToVariableTypes (Rep a) VariableType) => a -> VariableTypes
--     toVariableTypes x = gtoVariableTypes (from x)

-- instance ToVariableTypes () where
--     toVariableTypes _ = V.empty

-- class GToVariableTypes a f where
--     gtoVariableTypes :: a p -> V.Vector f

-- instance ToVariableType a => GToVariableTypes (K1 i a) VariableType where
--     gtoVariableTypes (K1 a) = V.fromList [toVariableType a]

-- instance GToVariableTypes a f => GToVariableTypes (M1 i c a) f where
--     gtoVariableTypes (M1 a) = gtoVariableTypes a

-- instance (GToVariableTypes a f, GToVariableTypes b f) => GToVariableTypes (a :*: b) f where
--     gtoVariableTypes (a :*: b) = gtoVariableTypes a <> gtoVariableTypes b

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
deriving via (MyApplicative (VS.Vector n) a) instance (Num a) => ToVariableType (VS.Vector n a)

instance ToVariableType (MyMatrix a) where
    toVariableType _ = FieldType
deriving via (MyMatrix (DECrepresentationMatrix n l c1 k1 c2 k2 a)) instance ToVariableType (DECrepresentationMatrix n l c1 k1 c2 k2 a)
deriving via (MyMatrix (MSL.SparseMatrix p1 p2 a)) instance ToVariableType (MSL.SparseMatrix p1 p2 a)
deriving via (MyMatrix (MSL.Matrix p1 p2 a)) instance ToVariableType (MSL.Matrix p1 p2 a)
