module Test.Matrix where

import qualified Data.Matrix.Static.LinearAlgebra as MS
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Matrix.Class

a = one :: MS.SparseMatrix 2 2 Double
b = one :: MS.SparseMatrix 2 0 Double
unit_MSmul = print $ a .@. b