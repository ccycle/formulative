{-# OPTIONS_GHC-fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC-fplugin GHC.TypeLits.Normalise #-}
module Test.Homology where

import Data.Coerce
import qualified Data.Matrix.Static.LinearAlgebra as MSL
import qualified Data.Matrix.Static.LinearAlgebra.Types as MSL
import qualified Data.Matrix.Static.Sparse as MSS
import Data.Proxy
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import HStructure.Calculation.DEC.Homology
import HStructure.Calculation.Internal.TypeLevelList

type NatList = '[1, 4, 5, 2]
scTest3 = MkSimplicialComplex @3 @NatList $ S.fromList $ Prelude.map (MkSimplex . V.fromList) $ [[0, 1, 2], [1, 3, 2]]
scTest2 = generateSimplicialKComplexToKminus1 scTest3
scTest1 = generateSimplicialKComplexToKminus1 scTest2
scTest0 = generateSimplicialKComplexToKminus1 scTest1

-- scTest0Err = generateSimplicialKComplexToKminus1 scTest0
cooTest2 = boundaryOperatorMatrixCOOList scTest3 scTest2 :: VU.Vector (Index, Index, Double)
cooTest1 = boundaryOperatorMatrixCOOList scTest2 scTest1 :: VU.Vector (Index, Index, Double)
cooTest0 = boundaryOperatorMatrixCOOList scTest1 scTest0 :: VU.Vector (Index, Index, Double)
dMatTest2 = MSS.fromTriplet cooTest2 :: MSL.SparseMatrix (NatList !! 3) (NatList !! 2) Double
dMatTest1 = MSS.fromTriplet cooTest2 :: MSL.SparseMatrix (NatList !! 2) (NatList !! 1) Double
dMatTest0 = MSS.fromTriplet cooTest0 :: MSL.SparseMatrix (NatList !! 1) (NatList !! 0) Double

length_scTest2 = length $ unMkSimplicialComplex scTest2
dMatProdTest = dMatTest2 MSL.@@ dMatTest1

unit_scTest2 = print scTest2
unit_scTest1 = print scTest1
unit_dMatTest2 = print dMatTest2
unit_dMatTest1 = print dMatTest1
unit_length_scTest2 = print length_scTest2
unit_dMatProdTest = print dMatProdTest