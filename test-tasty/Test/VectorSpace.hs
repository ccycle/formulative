{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.VectorSpace where

import Data.VectorSpace
import Data.Void
import GHC.Generics

-- package "vectorspace" のテスト

type VRep v = Rep v Void

class (VectorSpace v) => NormSpace v where
    type NScalar v :: *
    type NScalar v = NScalar (VRep v)
    norm :: v -> NScalar v
    default norm :: (Generic v, NormSpace (VRep v), NScalar (VRep v) ~ NScalar v) => v -> NScalar v
    norm v = norm (from v :: VRep v)

instance NormSpace a => NormSpace (Rec0 a s) where
    type NScalar (Rec0 a s) = NScalar a
    norm (K1 w) = norm w

instance NormSpace (f p) => NormSpace (M1 i c f p) where
    type NScalar (M1 i c f p) = NScalar (f p)
    norm (M1 w) = norm w

instance
    ( NormSpace (f p)
    , NormSpace (g p)
    , Scalar (f p) ~ Scalar (g p)
    , NScalar (f p) ~ NScalar (g p)
    , Ord (NScalar (f p))
    ) =>
    NormSpace ((f :*: g) p)
    where
    type NScalar ((f :*: g) p) = NScalar (f p)
    norm (x :*: y) = max (norm x) (norm y)

instance {-# OVERLAPS #-} NormSpace Double where
    type NScalar Double = Double
    norm = abs

data RecVec = RecVec {pressure :: Double, density :: Double} deriving (Show, Generic, AdditiveGroup, VectorSpace, InnerSpace, NormSpace)

unit_test1 = print $ RecVec 1 1
unit_test2 = print $ RecVec 1 1 <.> RecVec 2 3
unit_testNorm = print $ norm $ RecVec 1 2
