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
import Formulative.Calculation.Algebra.Arithmetic.Class
import GHC.Exts
import GHC.TypeNats

-- import Formulative.Calculation.DiscreteExteriorCalculus.Class
-- import Formulative.Calculation.DiscreteExteriorCalculus.Homology

import Formulative.Calculation.DiscreteExteriorCalculus.Homology.Operators
import Formulative.Calculation.DiscreteExteriorCalculus.Homology.Types
import Formulative.Calculation.Internal.Infix
import Formulative.Calculation.Internal.TypeLevelList

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
-- Simplices (fromList [Simplex [0,1],Simplex [0,2],Simplex [0,3],Simplex [1,2],Simplex [1,3],Simplex [1,4],Simplex [2,3],Simplex [2,4],Simplex [3,4]])