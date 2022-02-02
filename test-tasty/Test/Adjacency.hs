-- {-# OPTIONS_GHC-fplugin GHC.TypeLits.KnownNat.Solver #-}
-- {-# OPTIONS_GHC-fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE OverloadedLists #-}

module Test.Adjacency where

import Control.Carrier.Reader
import Control.Effect.Sum
import Data.Coerce
import qualified Data.Matrix.Static.LinearAlgebra as MSL
import qualified Data.Matrix.Static.LinearAlgebra.Types as MSL
import qualified Data.Matrix.Static.Sparse as MSS
import Data.Proxy
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import GHC.Exts
import GHC.TypeNats
import HStructure.Calculation.Algebra.Arithmetic.Class
import HStructure.Calculation.DiscreteExteriorCalculus.Class
import HStructure.Calculation.DiscreteExteriorCalculus.Homology
import HStructure.Calculation.Internal.Infix
import HStructure.Calculation.Internal.TypeLevelList

type NEuc = 3
type NatsTest = '[5, 9, 2]

scTest3 = [[0, 1, 2, 3], [1, 2, 3, 4]] :: Simplices NEuc NatsTest
scTest2 = generateSimplicialKComplexToKminus1 scTest3
scTest1 = generateSimplicialKComplexToKminus1 scTest2

simplex1Test1 = [0, 1] :: Simplex 1
simplex1Test2 = [0, 2] :: Simplex 1
simplex3Test1 = [0, 1, 2, 3] :: Simplex 3

-- >>>  adjacencyList scTest1 simplex3Test1
-- [0,1,2,3,4,6]
-- >>> scTest1
-- MkSimplices (fromList [MkSimplex [0,1],MkSimplex [0,2],MkSimplex [0,3],MkSimplex [1,2],MkSimplex [1,3],MkSimplex [1,4],MkSimplex [2,3],MkSimplex [2,4],MkSimplex [3,4]])