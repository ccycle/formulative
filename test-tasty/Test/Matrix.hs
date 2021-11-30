module Test.Matrix where

import qualified Data.Matrix.Static.LinearAlgebra as MS
import HStructure.Calculation.Algebra.Arithmetic.Class
import HStructure.Calculation.Matrix.Class

a = one :: MSSparseMatrix 2 2 Double
b = one :: MSSparseMatrix 2 0 Double
unit_MSmul = print $ a @*@ b