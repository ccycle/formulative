{-# OPTIONS_GHC-fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC-fplugin GHC.TypeLits.Normalise #-}
module Test.Homology where

import Data.Coerce
import qualified Data.Matrix.Static.LinearAlgebra as MSL
import qualified Data.Matrix.Static.LinearAlgebra.Types as MSL
import qualified Data.Matrix.Static.Sparse as MSS
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as VU
import HStructure.Calculation.DEC.Homology

scTest2 = MkSimplicialComplex $ S.fromList $ Prelude.map (coerce @[Int] @(Simplex 3)) [[0, 1, 2], [1, 3, 2]]
scTest1 = generateKSimplicialComplexToKminus1 scTest2
scTest0 = generateKSimplicialComplexToKminus1 scTest1
scTestphi = generateKSimplicialComplexToKminus1 scTest0
scTestphiErr = generateKSimplicialComplexToKminus1 scTestphi
cooTest1 = boundaryOperatorMatrixCOOList scTest2 scTest1 :: VU.Vector (Index, Index, Double)
cooTest0 = boundaryOperatorMatrixCOOList scTest1 scTest0 :: VU.Vector (Index, Index, Double)
cooTestPhi = boundaryOperatorMatrixCOOList scTest0 scTestphi :: VU.Vector (Index, Index, Double)
dMatTest1 = MSS.fromTriplet cooTest1 :: MSL.SparseMatrix 2 5 Double
dMatTest0 = MSS.fromTriplet cooTest0 :: MSL.SparseMatrix 5 4 Double

length_scTest1 = length $ unMkSimplicialComplex scTest1
dMatProdTest = dMatTest1 MSL.@@ dMatTest0

unit_scTest1 = print scTest1
unit_scTest0 = print scTest0
unit_dMatTest1 = print dMatTest1
unit_dMatTest0 = print dMatTest0
unit_length_scTest1 = print length_scTest1
unit_dMatProdTest = print dMatProdTest