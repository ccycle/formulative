module Test.Geometry where

import Data.Constraint
import Data.Proxy
import Data.Singletons
import Formulative.Calculation.Algebra.Arithmetic
import Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Proofs
import Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Types
import Formulative.Calculation.DiscreteExteriorCalculus.Geometry.Operators
import Formulative.Calculation.DiscreteExteriorCalculus.Geometry.Types
import Formulative.Calculation.DiscreteExteriorCalculus.Homology.Types
import Formulative.Calculation.Internal.List
import Formulative.Calculation.Matrix.Class
import GHC.TypeNats

-- point 0: [0, 0, 0]
-- point 1: [1, 0, 1]
-- point 2: [1, 1, 0]
-- point 3: [0, 1, 1]
--
-- x0List : [0,1,1,0]
-- x1List : [0,0,1,1]
-- x2List : [0,1,0,1]
allPositions3dTest :: AllPointDataPrimal0 3 4 Double
allPositions3dTest = AllPointDataPrimal0 $ fromList $ map fromList [[0, 1, 1, 0], [0, 0, 1, 1], [0, 1, 0, 1]]

s2Test :: Simplex 2
s2Test = fromList [0, 1, 2]

s2Mat3d = simplexToPositionMat allPositions3dTest s2Test

sMatUn = unPositionMatrix s2Mat3d

sMatG = sMatUn .@. transpose sMatUn

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
allPositions2dTest = AllPointDataPrimal0 $ fromList $ map fromList [[0, 0, 1], [0, 1, 1]]
s2Mat2d = simplexToPositionMat allPositions2dTest s2Test
circumcenterTest2d = circumcenterInternalUnsafe s2Mat2d
primalVolumeTest2d = primalVolumeInternal allPositions2dTest s2Test

-- case 3: obtuse angle
allPositions2dTestObtuseAngle :: AllPointDataPrimal0 2 3 Double
allPositions2dTestObtuseAngle = AllPointDataPrimal0 $ fromList $ map fromList [[0, sqrt 3, sqrt 3 / 2], [0, 0, 1 / 2]]
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