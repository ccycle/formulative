module Formulative.Calculation.Internal.List (
    module GHC.Exts,
    module Formulative.Calculation.Internal.List,
) where

import Data.List
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as VA
import qualified Data.Vector.Sized as VS
import qualified Data.Vector.Storable as VST
import qualified Data.Vector.Unboxed as VU
import GHC.Exts (IsList (Item, fromList, toList))
import GHC.TypeNats
import Numeric.LinearAlgebra (Element)
import Numeric.LinearAlgebra.Data ((><))
import Numeric.LinearAlgebra.HMatrix (fromLists, toLists)

singleton :: a -> [a]
singleton x = [x]

safeListCall :: Foldable t => (t a -> b) -> t a -> Maybe b
safeListCall f xs
    | null xs = Nothing
    | otherwise = Just $ f xs

headMaybe :: [a] -> Maybe a
headMaybe = safeListCall head

tailMaybe :: [a] -> Maybe [a]
tailMaybe = safeListCall tail

initMaybe :: [a] -> Maybe [a]
initMaybe = safeListCall Data.List.init

class (IsList a) => IsListMaybe a where
    fromListMaybe :: [Item a] -> Maybe a

class IsVector v1 v2 where
    fromVector :: v1 -> v2
    toVector :: v2 -> v1

-- >>> :set -XOverloadedLists
-- >>> [0,1,2,3,4,5] :: MSL.SparseMatrix 3 2 Double
-- instance (KnownNat m, KnownNat n, VST.Storable a, MSS.Zero a) => IsList (MSL.SparseMatrix m n a) where
--     type Item (MSL.SparseMatrix m n a) = a
--     fromList = MSS.fromList
--     toList = MSS.toList

instance (KnownNat n) => IsList (VS.Vector n a) where
    type Item (VS.Vector n a) = a
    fromList = fromJust . VS.fromList
    toList = VS.toList

-- TODO: 失敗可能性を考慮したクラスを用意(IsListMaybe？)
-- instance (KnownNat m, KnownNat n, E.Elem a) => IsList (HMatrixSized m n a) where
--     type Item (HMatrixSized m n a) = [a]
--     fromList = fromJust . E.fromList
--     toList = E.toList

class MapClass t a where
    mapG :: (a -> b) -> t a -> t b
instance MapClass (VS.Vector n) a where
    mapG = VS.map
instance MapClass V.Vector a where
    mapG = V.map

-- unsafe index
class (IsList a) => UnsafeIndex a where
    unsafeIndex :: a -> Int -> Item a
instance UnsafeIndex [a] where
    unsafeIndex = (!!)
instance (KnownNat n) => UnsafeIndex (VS.Vector n a) where
    unsafeIndex = VS.unsafeIndex
instance UnsafeIndex (V.Vector a) where
    unsafeIndex = V.unsafeIndex
instance (Ord a) => UnsafeIndex (Set a) where
    unsafeIndex = flip S.elemAt

class (IsList a) => SafeIndex a where
    safeIndex :: a -> Int -> Maybe (Item a)

-- instance SafeIndex [a] where
--     safeIndex = (!!)
-- instance SafeIndex (VS.Vector n a) where
--     safeIndex = VS.unsafeIndex
-- instance SafeIndex (V.Vector a) where
--     safeIndex = V.unsafeIndex
-- instance SafeIndex (Set a) where
--     safeIndex = flip S.elemAt
-- instance (MSG.Matrix mat v a) => SafeIndex (mat r 1 v a) where
--     safeIndex mat i = MSD.unsafeIndex mat (i, 0)

class ZipWithV (t :: * -> *) where
    zipWithV :: (a -> b -> c) -> t a -> t b -> t c
instance ZipWithV [] where
    zipWithV = zipWith
instance ZipWithV V.Vector where
    zipWithV = V.zipWith
instance ZipWithV (VS.Vector n) where
    zipWithV = VS.zipWith

class LengthV a where
    lengthV :: a -> Int
instance (KnownNat n) => LengthV (VS.Vector n a) where
    lengthV = VS.length
instance LengthV (V.Vector a) where
    lengthV = V.length
instance LengthV (Set a) where
    lengthV = S.size

class EmptyV a where
    emptyV :: a
instance EmptyV [a] where
    emptyV = []
instance EmptyV (V.Vector a) where
    emptyV = V.empty
instance (VU.Unbox a) => EmptyV (VU.Vector a) where
    emptyV = VU.empty
instance EmptyV (VS.Vector 0 a) where
    emptyV = VS.empty

class Cons a b where
    cons :: a -> b -> b
    snoc :: b -> a -> b
    snoc = flip cons

class SortV (t :: * -> *) where
    -- gSortBy :: (a -> a -> Ordering) -> t a -> t a
    -- gSort = gSortBy compare
    sortV :: (Ord a) => t a -> t a
instance SortV [] where
    sortV = sort
instance SortV V.Vector where
    sortV = V.modify VA.sort