{-# LANGUAGE RebindableSyntax #-}

module Formulative.Calculation.DifferentialEquation.TimeEvolution where

import Formulative.Calculation.Algebra.Arithmetic
import Formulative.Calculation.DifferentialEquation.Dynamics.Class
import Formulative.Calculation.DifferentialEquation.Dynamics.Effect
import Formulative.Calculation.DifferentialEquation.Types
import Formulative.Calculation.VectorSpace.Class
import Prelude hiding (fromInteger)

eulerMethod (StepSize dt) f x = x .+. dt *. f x
eulerMethodM x = do
  dt <- askStepSize
  f <- evolutionFunctionM
  return $ eulerMethod dt f x

rungeKutta4Method (StepSize dt) f x = s
 where
  k1 = f x
  k2 = f (x .+. (dt ./. 2) *. k1)
  k3 = f (x .+. (dt ./. 2) *. k2)
  k4 = f (x .+. dt *. k3)
  s = (dt ./. 6) *. (k1 .+. 2 *. k2 .+. 2 *. k3 .+. k4)
rungeKutta4MethodM x = do
  dt <- askStepSize
  f <- evolutionFunctionM
  return $ rungeKutta4Method dt f x