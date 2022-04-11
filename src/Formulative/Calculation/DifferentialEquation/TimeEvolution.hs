{-# LANGUAGE RebindableSyntax #-}

module Formulative.Calculation.DifferentialEquation.TimeEvolution where

import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.DifferentialEquation.Types
import Formulative.Calculation.VectorSpace.Class

eulerMethod (MkStepSize dt) f x = x .+. dt *. f x

rungeKutta4Method (MkStepSize dt) f x = s
  where
    k1 = f x
    k2 = f (x .+. (dt ./. 2) *. k1)
    k3 = f (x .+. (dt ./. 2) *. k2)
    k4 = f (x .+. dt *. k3)
    s = (dt ./. 6) *. (k1 .+. 2 *. k2 .+. 2 *. k3 .+. k4)