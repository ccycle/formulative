module Test.EigenSparseMatrixTest where

import qualified Eigen.Matrix as E
import qualified Eigen.SparseMatrix as ES
import Test.Tasty

-- import GHC.TypeNats
import Formulative.Calculation.Algebra.Arithmetic.Class

-- import Formulative.Calculation.Calculus.Variation
import Formulative.Calculation.Matrix.Class
import Formulative.Calculation.VectorSpace.Class

mat1 = ES.fromList (zipWith (\i x -> (i, 0, x)) [0 .. 3] [2, 3, 5, 7]) :: ES.SparseMatrix 4 1 Double
mat2 = ES.fromList (zipWith (\i x -> (i, 0, x)) [0 .. 3] [11, 13, 17, 19]) :: ES.SparseMatrix 4 1 Double

-- instance (E.Elem a, Field a, KnownNat n, KnownNat m) => Multiplicative (ES.SparseMatrix n m a) where
--     (.*.) x y = mult ES.map x y
--     one = undefined

matAdd = mat1 .+. mat2
matSub = mat1 .-. mat2
matMul = mat1 .*. mat2

unit_addTest = print matAdd