{-# OPTIONS_GHC-fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC-fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.DiscreteExteriorCalculus.Geometry.Types where

import Data.Coerce
import Data.Hashable
import qualified Data.Matrix.Static.LinearAlgebra.Types as MSL
import Data.Singletons.TH hiding (type (<=))
import qualified Data.Vector.Sized as VS
import Dhall
import Formulative.Calculation.Algebra.Arithmetic.Additive
import Formulative.Calculation.Internal.TypeLevelList
import Formulative.Calculation.Matrix.Class
import Formulative.Calculation.Matrix.Types
import GHC.TypeNats

type EucDim = Nat

newtype DimensionOfEuclideanSpace = DimensionOfEuclideanSpace Natural
    deriving stock (Generic, Show, Eq)
    deriving newtype (Enum, Num)
    deriving anyclass (FromDhall, ToDhall, Hashable)

type GMatrixContainer k1 k2 a = HMatrixSized k1 k2 a
newtype GMatrix k a = GMatrix (GMatrixContainer k k a)
    deriving stock (Generic, Show)
unGMatrix :: GMatrix k a -> GMatrixContainer k k a
unGMatrix = coerce
type SizedVector n a = VS.Vector n a

-- data PointData (nEuc :: EucDim) (p :: Nat) a (m :: Type -> Type) k where
--     GetPointData :: PointData nEuc p a m (AllPointDataPrimal0 nEuc p a)
-- getPointData :: (Has (PointData n p a) sig m) => m (AllPointDataPrimal0 n p a)
-- getPointData = send GetPointData

-- data SomePointData a m k = forall nEuc p. SomePointData (PointData nEuc p a m k)

-- connectivity + point -> metric

-- data Metric (n :: Dim) l a m k where
--   GetMetric :: Proxy k -> SCellType c -> Metric n l a m (SizedVector nEuc (SizedVector (ToMatSize nBase l c k) a))

-- https://blog.jle.im/entry/introduction-to-singletons-2.html
$( singletons
    [d|
        data CellType = Primal | Dual | CEmpty
            deriving (Show, Eq)
        |]
 )

deriving stock instance Generic CellType
deriving anyclass instance FromDhall CellType
deriving anyclass instance ToDhall CellType
deriving anyclass instance Hashable CellType

type family ToMatSize (n :: Nat) (l :: [Nat]) (c :: CellType) (k :: Nat) where
    ToMatSize n l 'Primal k = l !! k
    ToMatSize n l 'Dual k = l !! DualDeg n k
    ToMatSize n l 'CEmpty k = k

type SuccDeg (n :: Nat) (k :: Nat) = (k + 1)
type PredDeg (n :: Nat) (k :: Nat) = (k - 1)

type DualDeg (n :: Nat) (k :: Nat) = (n - k)
type DualSuccDeg (n :: Nat) (k :: Nat) = DualDeg n (SuccDeg n k)
type DualPredDeg (n :: Nat) (k :: Nat) = DualDeg n (PredDeg n k)

type SizedMatrix p1 p2 a = MSL.SparseMatrix p1 p2 a -- 実装に使う型を選択

newtype AllPointData nEuc nBase l c k a = AllPointData (SizedVector nEuc (SizedVector (ToMatSize nBase l c k) a))
    deriving stock (Show, Eq)
newtype AllPointDataPrimal0 nEuc p a = AllPointDataPrimal0 (SizedVector nEuc (SizedVector p a))
    deriving (Show, Eq)
newtype PositionMatrix nEuc k a = PositionMatrix {unPositionMatrix :: GMatrixContainer (k + 1) nEuc a}
    deriving stock (Show)
newtype AllPositionMatrix nEuc p a = AllPositionMatrix {unAllPositionMatrix :: SizedMatrix p nEuc a}
    deriving stock (Show, Eq)
    deriving newtype (Additive)
newtype BarycentricCoordinate k a = BarycentricCoordinate (GMatrixContainer k 1 a)
    deriving stock (Show)

newtype Circumcenter n a = Circumcenter (GMatrixContainer 1 n a)
    deriving stock (Show)
newtype Circumradius a = Circumradius a deriving (Show)
newtype AllCircumcenter n l k a = AllCircumcenter (SizedVector (l !! k) (Circumcenter n a)) deriving (Show)
newtype CircumcentersOfSimplex nEuc l k a = CircumcenterSimplex {unCircumcentersVector :: SizedMatrix (l !! k) 1 a} deriving (Eq, Show)

type Component = Double

newtype Volume c k a = Volume a deriving (Show)
newtype AllVolume n l c k a = AllVolume (SizedVector (l !! k) (Circumcenter n a)) deriving (Show)
