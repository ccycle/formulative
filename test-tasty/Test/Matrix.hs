module Test.Matrix where

import qualified Data.Matrix.Static.LinearAlgebra as MS
import HStructure.Calculation.Algebra.Arithmetic.Class
import HStructure.Calculation.Matrix.Class

a = one :: MS.SparseMatrix 2 2 Double
b = one :: MS.SparseMatrix 2 0 Double
unit_MSmul = print $ a .@. b