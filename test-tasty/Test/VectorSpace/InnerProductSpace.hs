module Test.VectorSpace.InnerProductSpace where

import Data.Complex
import qualified Data.Matrix.Static.LinearAlgebra.Types as MSL
import Formulative.Calculation.Matrix.Class
import Formulative.Calculation.VectorSpace.Class
import GHC.Exts (IsList (fromList))

a = fromList [1 :+ 1, 1 :+ 0] :: MSL.SparseMatrix 2 1 (Complex Double)
b = fromList [1 :+ 0, 1 :+ 0] :: MSL.SparseMatrix 2 1 (Complex Double)

innerProductTest1 = a <.> b
innerProductTest2 = b <.> a

-- normTest1 = normL1 a
-- normTest2 = normL2 a

-- conjugate a <.> b = b <.> a