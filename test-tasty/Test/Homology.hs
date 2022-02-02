-- {-# OPTIONS_GHC-fplugin GHC.TypeLits.KnownNat.Solver #-}
-- {-# OPTIONS_GHC-fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE OverloadedLists #-}

module Test.Homology where

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
import OptDEC.Calculation.Algebra.Arithmetic.Class
import OptDEC.Calculation.DiscreteExteriorCalculus.Class
import OptDEC.Calculation.DiscreteExteriorCalculus.Homology
import OptDEC.Calculation.Internal.Infix
import OptDEC.Calculation.Internal.TypeLevelList

type NEuc = 2
type NatsTest = '[4, 5, 2]

scTest2 = [[0, 1, 2], [1, 3, 2]] :: Simplices NEuc NatsTest
scTest1 = generateSimplicialKComplexToKminus1 scTest2
scTest0 = generateSimplicialKComplexToKminus1 scTest1
scTest3 = generateSimplicialKComplexFromN (Proxy :: Proxy 3) scTest2

dual0formTest = one :: DifferentialForm NEuc NatsTest Dual 0 Double
dual1formTest = one :: DifferentialForm NEuc NatsTest Dual 1 Double
primal0formTest = one :: DifferentialForm NEuc NatsTest Primal 0 Double
primal1formTest = one :: DifferentialForm NEuc NatsTest Primal 1 Double

-- scTest0Err = generateSimplicialKComplexToKminus1 scTest0
-- cooTest2 = boundaryOperatorMatrixCOOList scTest3 scTest2 :: VU.Vector (Index, Index, Double)
-- cooTest1 = boundaryOperatorMatrixCOOList scTest2 scTest1 :: VU.Vector (Index, Index, Double)
-- cooTest0 = boundaryOperatorMatrixCOOList scTest1 scTest0 :: VU.Vector (Index, Index, Double)
-- dMatTest2 = MSS.fromTriplet cooTest2 :: MSL.SparseMatrix (NatsTest !! 3) (NatsTest !! 2) Double
-- dMatTest1 = MSS.fromTriplet cooTest2 :: MSL.SparseMatrix (NatsTest !! 2) (NatsTest !! 1) Double
-- dMatTest0 = MSS.fromTriplet cooTest0 :: MSL.SparseMatrix (NatsTest !! 1) (NatsTest !! 0) Double

d0test = exteriorDerivativePrimalInternal scTest1 scTest0 :: ExteriorDerivative NEuc NatsTest Primal 0 Double
d1test = exteriorDerivativePrimalInternal scTest2 scTest1 :: ExteriorDerivative NEuc NatsTest Primal 1 Double
d2test = exteriorDerivativePrimalInternal scTest3 scTest2 :: ExteriorDerivative NEuc NatsTest Primal 2 Double
d3test = zero :: ExteriorDerivative NEuc NatsTest Primal 3 Double

inclusionMapTest = inclusionMapMatrixInternal @_ @_ @Primal @Double scTest2

length_scTest2 = length $ unMkSimplicialComplex scTest2

dProdTest0 = d1test .@. d0test
dProdTest1 = d2test .@. d1test

inclusionMapTypeAnnotationTest ::
    (Algebra sig m, Has (Reader (Simplices NEuc NatsTest)) sig m) =>
    m (InclusionMap NEuc NatsTest 'Primal Double)
inclusionMapTypeAnnotationTest = inclusionMapMat

iStarTest :: (HasInclusionMap NEuc NatsTest Primal Double sig m) => m (DifferentialForm NEuc NatsTest Primal 1 Double)
iStarTest = do
    i <- inclusionMap
    i' <- inclusionMap
    return $ i' $ i primal1formTest

dPolymorphicTest :: (HasInclusionMap NEuc NatsTest Primal Double sig m) => m (DifferentialForm NEuc NatsTest Primal 3 Double)
dPolymorphicTest = do
    d1 <- exteriorDerivative
    d2 <- exteriorDerivative
    return $ d2 . d1 $ primal1formTest

-- error (KnownNat solverの問題？)
-- dPolymorphicTest2 :: (HasInclusionMap NEuc NatsTest Primal Double sig m) => m (DifferentialForm NEuc NatsTest Primal 3 Double)
-- dPolymorphicTest2 = (exteriorDerivative >*> exteriorDerivative) <$> pure primal1formTest

-- d <- exteriorDerivative
-- return $ d . d $ primal1formTest

-- 型推論がうまく行かない
-- inclusionMapEffTest = run . runReader scTest2 $ inclusionMapMat

inclusionMapEffMulTest ::
    forall sig m.
    (Algebra sig m, Has (Reader (Simplices NEuc NatsTest)) sig m) =>
    m (DifferentialForm NEuc NatsTest 'Primal (NEuc - 1) Double)
inclusionMapEffMulTest = do
    -- i <- inclusionMapTypeAnnotationTest
    -- return $ i .@. primal1formTest
    inclusionMapTypeAnnotationTest <.@.> pure primal1formTest

inclusionMapEffTest = run . runReader scTest2 $ inclusionMapMat @NEuc @NatsTest @Primal @Double

-- inclusionMapFunctEffTest   ::  forall sig m.
--     (Algebra sig m, Has (Reader (Simplices NEuc NatsTest)) sig m) =>
--     m (DifferentialForm NEuc NatsTest 'Primal (NEuc - 1) Double -> DifferentialForm NEuc NatsTest 'Primal (NEuc - 1) Double)
inclusionMapFunctEffTest :: (HasInclusionMap NEuc NatsTest Primal Double sig m) => m (DifferentialForm NEuc NatsTest Primal (NEuc -1) Double -> DifferentialForm NEuc NatsTest Primal (NEuc -1) Double)
inclusionMapFunctEffTest = do
    i <- inclusionMapMat @NEuc @NatsTest @Primal @Double
    return $ \x -> i .@. x
d1Dualtest = exteriorDerivativeDualInternal (Proxy :: Proxy 1) d0test

-- d1DualtestNotAnnotated = exteriorDerivativeDualInternal d0test

-- d2Dualtest = exteriorDerivativeDualInternal (Proxy :: Proxy 2) d1test

dualMulTest = d1Dualtest .@. dual1formTest

-- d1DualDualtest = exteriorDerivativeDualInternal @_ @_ @_ @0 d1Dualtest

-- unit tests
unit_scTest2 = print scTest2
unit_scTest1 = print scTest1

unit_dMatTest2 = print d2test
unit_dMatTest1 = print d1test
unit_length_scTest2 = print length_scTest2

unit_dProdTest0 = print dProdTest0
unit_dProdTest1 = print dProdTest1

unit_inclusionMapProdTest = print $ inclusionMapEffTest .@. d0test