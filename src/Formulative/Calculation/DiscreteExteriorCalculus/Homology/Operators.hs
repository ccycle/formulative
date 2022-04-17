{-# OPTIONS_GHC-fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC-fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.DiscreteExteriorCalculus.Homology.Operators where

import Control.Algebra
import Control.Monad
import Control.Monad.ST.Strict
import Data.Coerce
import Data.Foldable (Foldable (foldl'))
import qualified Data.IntMap.Strict as M
import Data.Proxy
import Data.STRef.Strict
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Unboxed as VU
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.DiscreteExteriorCalculus.Homology.Effect
import Formulative.Calculation.DiscreteExteriorCalculus.Homology.Types
import Formulative.Calculation.Internal.List
import GHC.Exts (IsList (Item, fromList, toList))
import GHC.TypeNats

signOfIndexListInternal :: SimplexInternal -> Index
signOfIndexListInternal list = g (toList list)
 where
  g (x : xs) = g xs * product (fmap (signum . subtract x) xs)
  g _ = 1

-- >>> signOfIndexList $ Simplex @3 [0,1,3]
-- 1
-- >>> signOfIndexList $ Simplex @3 [1,0,3]
-- -1
-- >>> signOfIndexList $ Simplex @3 [1,1,3]
-- 0
signOfIndexList :: Simplex k -> Index
signOfIndexList (Simplex list) = signOfIndexListInternal (coerce list)

deleteAtInternal :: Int -> SimplexInternal -> SimplexInternal
deleteAtInternal idx xs = lft V.++ tailVN rgt
 where
  (lft, rgt) = V.splitAt idx xs
  tailVN list = if V.null list then V.empty else V.tail list

-- >>> deleteAtS 1 [2,3,5,7]
-- [2,5,7]
-- >>> deleteAtS 2 [2,3,5,7]
-- [2,3,7]
-- deleteAtS :: forall n m a. (KnownNat n, KnownNat m) =>
--   Proxy n ->VS.Vector (n + m) a -> VS.Vector (n + m - 1) a
deleteAtS :: forall n a. Int -> Simplex (n + 1) -> Simplex n
deleteAtS idx = coerce . deleteAtInternal idx . coerce

removeIndexInternal :: SimplexInternal -> SimplexContainerInternal SimplexInternal
removeIndexInternal x = V.map (`deleteAtInternal` x) $ V.enumFromN 0 kInt
 where
  kInt = lengthV x

concatVec :: Foldable t => t (V.Vector a) -> V.Vector a
concatVec = foldl' (<>) (fromList [])

-- >>> extractSubSimplexInternal $ removeIndexInternal ([0,1,2,3] :: SimplexInternal)
-- [[0,1],[0,2],[0,3],[1,2],[1,3],[2,3]]
extractSubSimplexInternal = V.uniq . sortVec . V.map sortSimplexInternal . concatVec . V.map removeIndexInternal

-- >>> >>> splitToSubSimplexInternal 2 ([0,1,2,3] :: SimplexInternal)
-- [[0,1],[0,2],[0,3],[1,2],[1,3],[2,3]]
-- >>> splitToSubSimplexInternal 3 ([0,1,2,3] :: SimplexInternal)
-- [[0],[1],[2],[3]]
splitToSubSimplexInternal n s
  | n <= 0 = V.fromList [s]
  | n == 1 = removeIndexInternal s
  | otherwise = extractSubSimplexInternal $ splitToSubSimplexInternal (pred n) s

splitToSubSimplex :: forall k1 k2. (KnownNat k1, KnownNat k2, k1 <= k2) => Proxy k1 -> Simplex k2 -> SimplexContainer (Simplex k1)
splitToSubSimplex _ (Simplex s) = V.map Simplex $ splitToSubSimplexInternal (k2Int - k1Int) s
 where
  k1Int = natToInt (Proxy :: Proxy k1)
  k2Int = natToInt (Proxy :: Proxy k2)

-- adjacency list
adjacencyList ::
  forall k1 k2 l.
  (KnownNat k1, KnownNat k2, (k1 <= k2)) =>
  Simplices k1 l ->
  Simplex k2 ->
  (Simplex k2, AdjacencyKeys k1)
adjacencyList (Simplices sc) s = (s, AdjacencyKeys $ sortVec $ V.mapMaybe (adjacencySC sc) sList)
 where
  sList = splitToSubSimplex (Proxy :: Proxy k1) s
  sList1 = V.mapMaybe (adjacencySC sc) sList

-- Simplices k1 l -> Simplex k2 -> SimplexContainer Int
adjacencyListSC sc1 (Simplices sc2) = S.map (adjacencyList sc1) sc2

removeIndex :: Simplex k -> SimplexContainer (Simplex (k - 1))
removeIndex = coerce . removeIndexInternal . coerce

-- >>> removeIndex $ Simplex @3 [0,1,3]
-- [Simplex [1,3],Simplex [0,3],Simplex [0,1]]
-- removeIndex :: forall k. KnownNat k => Simplex (k + 1) -> SimplexContainer (k + 1) (Simplex k)
-- removeIndex x = VS.map (`deleteAtS` x) $ VS.enumFromN @(k + 1) @Int 0
--  where
--   kInt = natToInt (Proxy :: Proxy k)

-- deleteAtL idx xs = lft ++ rgt
--  where
--   (lft, _ : rgt) = L.splitAt idx xs

-- removeIndexNotSized :: forall k. KnownNat k => Simplex k -> [Simplex (k - 1)]
-- removeIndexNotSized x = Prelude.map (`deleteAtL` x) [0 .. (length x - 1)]

natToInt :: forall n. (KnownNat n) => Proxy n -> Int
natToInt = fromIntegral . natVal

-- not signed
-- >>> removeIndexToSet $ Simplex @3 [0,1,3]
-- fromList [Simplex [0,1],Simplex [0,3],Simplex [1,3]]
removeIndexToSet :: forall k. (KnownNat k) => Simplex (k + 1) -> Set (Simplex k)
removeIndexToSet = S.fromList . V.toList . removeIndex

removeIndexToSetInternal :: SimplexInternal -> Set SimplexInternal
removeIndexToSetInternal = S.fromList . V.toList . removeIndexInternal

-- >>> sortSimplex $ Simplex @3 [3,0,1]
-- Simplex [0,1,3]
sortSimplex :: Simplex k -> Simplex k
sortSimplex = Simplex . sortSimplexInternal . unSimplex

sortSimplexInternal :: SimplexInternal -> SimplexInternal
sortSimplexInternal = sortVec

sortVec :: Ord a => V.Vector a -> V.Vector a
sortVec = V.modify VA.sort

-- >>> scTest2 = S.fromList $ coerce @[[Int]] @[Simplex 3] [[0,1,2],[1,3,2]]
-- >>> generateKSimplicialComplexToKminus1 scTest2
-- fromList [Simplex [0,1],Simplex [0,2],Simplex [1,2],Simplex [1,3],Simplex [2,3]]
-- generateKSimplicialComplexToKminus1 :: forall k. KnownNat k => Simplices (k+1) -> Simplices k
-- generateKSimplicialComplexToKminus1 = coerce . S.unions . S.map (S.map sortSimplex . removeIndexToSet) . coerce

generateSetOfSimplicialKComplexToKminus1Internal :: Set SimplexInternal -> Set SimplexInternal
generateSetOfSimplicialKComplexToKminus1Internal = S.unions . S.map (S.map sortSimplexInternal . removeIndexToSetInternal)

generateSetOfSimplicialKComplexToKminus1 :: forall k. KnownNat k => Set (Simplex (k + 1)) -> Set (Simplex k)
generateSetOfSimplicialKComplexToKminus1 = S.unions . S.map (S.map sortSimplex . removeIndexToSet)

generateSimplicialKComplexToKminus1 :: forall k l. (KnownNat k) => Simplices (k + 1) l -> Simplices k l
generateSimplicialKComplexToKminus1 = coerce . S.unions . S.map (S.map sortSimplex . removeIndexToSet) . coerce

generateEmptySimplicialComplex :: forall k l. (KnownNat k) => Simplices k l
generateEmptySimplicialComplex = coerce $ S.fromList []

applyNTimes :: Integral b => b -> (a -> a) -> (a -> a)
applyNTimes n f
  | n <= 0 = id
  | otherwise = f . applyNTimes (n - 1) f

-- applyNTimesHNat :: KnownNat n => Proxy n -> (a -> a) -> (a -> a)
-- applyNTimesHNat n f
--   | n == 0 = id
--   | otherwise = f . applyNTimes (n - 1) f

generateSetOfSimplicialKComplexFromNInternal n = applyNTimes n generateSetOfSimplicialKComplexToKminus1Internal

generateSimplicialKComplexFromN :: forall n l k. (KnownNat n, KnownNat k) => Proxy k -> Simplices n l -> Simplices k l
generateSimplicialKComplexFromN _
  | 0 <= n' - k' = fromSimplicialComplexInternaltoSized . generateSetOfSimplicialKComplexFromNInternal (n' - k') . toInternalSimplicialComplex
  | otherwise = const generateEmptySimplicialComplex
 where
  n' = natToInt (Proxy :: Proxy n)
  k' = natToInt (Proxy :: Proxy k)

getKSimplicialComplex ::
  forall n l k sig m.
  ( Algebra sig m
  , KnownNat n
  , KnownNat k
  , Has (Connectivity n l) sig m
  ) =>
  m (Simplices k l)
getKSimplicialComplex =
  getPrimitiveSimplicies @n @l >>= \sc ->
    return $ generateSimplicialKComplexFromN (Proxy :: Proxy k) sc

getKSimplicialComplexEff :: forall k n l sig m. (KnownNat k, KnownNat n, Has (Connectivity n l) sig m) => Proxy k -> m (Simplices k l)
getKSimplicialComplexEff _ =
  generateSimplicialKComplexFromN (Proxy :: Proxy k) <$> getPrimitiveSimplicies @n

-- どっちのほうが効率いいのか？(sortしてunionするかunionしてsortするか)
-- unions . S.map (S.map sortSimplex . removeIndexToSet)
-- S.map sortSimplex . unions . S.map (removeIndexToSet)

-- >>> s3 = Simplex @3 (V.fromList [2,3,4])
-- >>> sSet3 = S.fromList $ coerce @V.Vector [Int]] @[Simplex 3] [[0,1,2],[1,3,2],[2,3,4]]
-- >>> adjacencySC s3 sSet3
-- Just 2
-- >>> adjacencySC (Simplex @3 [4,6,8]) sSet3
-- Nothing
-- >>> s0 = Simplex @0 []
-- >>> sSet0 = S.fromList $ coerce @[[Int]] @[Simplex 0] [[]]
-- >>> adjacencySC s0 sSet0
-- Nothing

-- emptySimplex :: VS.Vector 0 Index
emptySimplex = fromList []
adjacencySC ::
  forall k1 (k2 :: Dim) (k3 :: k1).
  Set (Simplex k2) ->
  Simplex k2 ->
  Maybe (AdjacencyKey k2)
adjacencySC sc s = if s == Simplex emptySimplex then Nothing else AdjacencyKey <$> S.lookupIndex (sortSimplex s) sc

-- TODO: forM_を使わない形に直す
boundaryOperatorMatrixCOOList (Simplices sck) (Simplices sckMinus1) = runST $ do
  cooLists <- newSTRef $ fromList []
  let pk = lengthV sck
  forM_ ([0 .. pk - 1] :: [Index]) $ \i -> do
    let elemsBound = removeIndex $ unsafeIndex sck i
    let n = lengthV elemsBound
    forM_ ([0 .. n - 1] :: [Int]) $ \k -> do
      let s = elemsBound `unsafeIndex` k
      let sSign = signOfIndexList s
      let adj = adjacencySC sckMinus1 s
      case adj of
        Just (AdjacencyKey adjVal) -> do
          let coo = (i, adjVal, fromIntegral $ integralToSign k sSign)
          modifySTRef cooLists (`VU.snoc` coo)
        Nothing -> return ()
  readSTRef cooLists