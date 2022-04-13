{-# LANGUAGE DeriveAnyClass #-}

module Formulative.Calculation.DiscreteExteriorCalculus.Homology.Types where

import Data.Coerce
import Data.Hashable
import qualified Data.IntMap.Strict as M
import Data.Set
import qualified Data.Set as S
import Data.Singletons (SingI)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import Dhall
import Formulative.Calculation.Internal.List
import Formulative.Preprocess.DefaultValue
import GHC.TypeNats

newtype DimensionOfManifold = DimensionOfManifold Natural
    deriving stock (Generic, Show, Eq)
    deriving newtype (Enum, Num)
    deriving anyclass (FromDhall, ToDhall, Hashable)

type Index = Int

type SimplexInternal = V.Vector Index
type SimplicesInternal = Set SimplexInternal
type SimplexContainer a = V.Vector a
type SimplexContainerInternal = V.Vector

newtype AdjacencyKey k1 = AdjacencyKey Index
    deriving stock (Show, Eq, Ord)
    deriving newtype (Num)
newtype AdjacencyKeys k1 = AdjacencyKeys (SimplexContainer (AdjacencyKey k1))
newtype AdjacencySet k1 k2 = AdjacencySet (M.IntMap (AdjacencyKeys k2))

-- Simplexの中身は向きの情報を持っているので、Setでは表現できない
newtype Simplex (k :: Dim) = Simplex SimplexInternal deriving (Show, Eq, Ord)
unSimplex :: Simplex k -> SimplexInternal
unSimplex = coerce

instance IsList (Simplex k) where
    type Item (Simplex k) = Index
    fromList = coerce . V.fromList
    toList = V.toList . coerce
instance UnsafeIndex (Simplices k l) where
    (Simplices sSet) `unsafeIndex` i = i `S.elemAt` sSet

type Dim = Nat
type SSizes = [Nat]
type KnownHDims = SingI

-- 各要素のindexを検索しやすいデータ構造を選ぶ
-- Map (Simplex k) Index のほうがいいかも？Simplexで検索をかけることが多いのと、追加および削除に強くなる可能性あり
newtype Simplices (k :: Dim) (l :: SSizes) = Simplices (Set (Simplex k)) deriving (Show, Eq)
unSimplicialComplex :: Simplices k l -> Set (Simplex k)
unSimplicialComplex = coerce

instance IsList (Simplices k l) where
    type Item (Simplices k l) = Simplex k
    fromList = coerce . S.fromList
    toList = S.toList . coerce

newtype BoundarySimplices (k :: Dim) (l :: SSizes) = BoundarySimplices (Set (Simplex k)) deriving (Show, Eq)
unBoundarySimplices :: BoundarySimplices k l -> Set (Simplex k)
unBoundarySimplices = coerce

toSizedSetOfSimplex :: forall k. (KnownNat k) => SimplicesInternal -> Set (Simplex k)
toSizedSetOfSimplex = S.map (coerce @SimplexInternal @(Simplex k))

toInternalSetOfSimplex :: forall k. (KnownNat k) => Set (Simplex k) -> Set SimplexInternal
toInternalSetOfSimplex = S.map coerce

fromSimplicialComplexInternaltoSized :: forall k l. (KnownNat k) => SimplicesInternal -> Simplices k l
fromSimplicialComplexInternaltoSized = Simplices . toSizedSetOfSimplex

toInternalSimplicialComplex :: forall k l. (KnownNat k) => Simplices k l -> SimplicesInternal
toInternalSimplicialComplex = toInternalSetOfSimplex . unSimplicialComplex

-- coordinate list: (row, column, value)
type COO a = (Index, Index, a)
type COOstorage a = VU.Vector (COO a)

newtype SparseMatrixCOO a = SparseMatrixCOO (COOstorage a)
unSparseMatrixCOO :: SparseMatrixCOO a -> COOstorage a
unSparseMatrixCOO = coerce