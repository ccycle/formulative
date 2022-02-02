module Test.VectorSpace.InnerProductSpace where

import Data.Complex
import qualified Data.Matrix.Static.LinearAlgebra.Types as MSL
import GHC.Exts (IsList (fromList))
import OptDEC.Calculation.Matrix.Class
import OptDEC.Calculation.VectorSpace.Class

a = fromList [1 :+ 1, 1 :+ 0] :: MSL.SparseMatrix 2 1 (Complex Double)
b = fromList [1 :+ 0, 1 :+ 0] :: MSL.SparseMatrix 2 1 (Complex Double)

innerProductTest1 = a <.> b
innerProductTest2 = b <.> a
normTest1 = normL1 a
normTest2 = normL2 a

-- conjugate a <.> b = b <.> a