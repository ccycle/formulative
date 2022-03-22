module Test.Geometry where

import Data.Constraint
import Data.Proxy
import Data.Singletons
import GHC.TypeNats
import OptDEC.Calculation.Algebra.Arithmetic.Class

import OptDEC.Calculation.DiscreteExteriorCalculus.Geometry.Types

-- import OptDEC.Calculation.DiscreteExteriorCalculus.Homology
-- import OptDEC.Calculation.DiscreteExteriorCalculus.Proofs

import OptDEC.Calculation.DiscreteExteriorCalculus.DifferentialForm.Proofs
import OptDEC.Calculation.DiscreteExteriorCalculus.DifferentialForm.Types
import OptDEC.Calculation.DiscreteExteriorCalculus.Geometry.Operators
import OptDEC.Calculation.DiscreteExteriorCalculus.Homology.Types
import OptDEC.Calculation.Internal.List
import OptDEC.Calculation.Matrix.Class

-- point 0: [0, 0, 0]
-- point 1: [1, 0, 1]
-- point 2: [1, 1, 0]
-- point 3: [0, 1, 1]
--
-- x0List : [0,1,1,0]
-- x1List : [0,0,1,1]
-- x2List : [0,1,0,1]
allPositions3dTest :: AllPointDataPrimal0 3 4 Double
allPositions3dTest = MkAllPointDataPrimal0 $ fromList $ map fromList [[0, 1, 1, 0], [0, 0, 1, 1], [0, 1, 0, 1]]

s2Test :: Simplex 2
s2Test = fromList [0, 1, 2]

s2Mat3d = simplexToPositionMat allPositions3dTest s2Test

sMatUnMk = unMkPositionMatrix s2Mat3d

sMatG = sMatUnMk .@. transpose sMatUnMk

gMatTest3d = circumcenterAMat s2Mat3d

circumcenterTest3d = circumcenterInternalUnsafe s2Mat3d

------------------------
-- case 2: wrap 3 points
------------------------
-- point 0: [0, 0]
-- point 1: [0, 0]
-- point 2: [0, 0]
--
-- x0List : [0,0,0]
-- x1List : [0,0,0]
-- x2List : [0,0,0]
allPositions2dTest :: AllPointDataPrimal0 2 3 Double
allPositions2dTest = MkAllPointDataPrimal0 $ fromList $ map fromList [[0, 0, 1], [0, 1, 1]]
s2Mat2d = simplexToPositionMat allPositions2dTest s2Test
circumcenterTest2d = circumcenterInternalUnsafe s2Mat2d
primalVolumeTest2d = primalVolumeInternal allPositions2dTest s2Test

-- case 3: obtuse angle
allPositions2dTestObtuseAngle :: AllPointDataPrimal0 2 3 Double
allPositions2dTestObtuseAngle = MkAllPointDataPrimal0 $ fromList $ map fromList [[0, sqrt 3, sqrt 3 / 2], [0, 0, 1 / 2]]
s2Mat2dObtuseAngle = simplexToPositionMat allPositions2dTestObtuseAngle s2Test
circumcenterTest2dObtuseAngle = circumcenterInternalUnsafe s2Mat2dObtuseAngle

primalVolumeTest3d = primalVolumeInternal allPositions3dTest s2Test

unit_sMatTest = print $ simplexToPositionMat allPositions3dTest s2Test

printDeg :: (KnownNat k) => Proxy k -> IO ()
printDeg proxyk = print (natVal proxyk)

printDualDeg :: forall n k. (KnownNat n, KnownNat k) => Proxy n -> Proxy k -> IO ()
printDualDeg n k = case dualDegDict n k of Dict -> print (natVal (Proxy :: Proxy (DualDeg n k)))

printDualMap :: forall c. (SingI c) => SCellType c -> IO ()
printDualMap s = case dualMapDict @c of Sub Dict -> print (fromSing (sing @(DualMap c)))