module Formulative.Calculation.DiscreteExteriorCalculus.Geometry.Submanifold.Types where

import Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Types

newtype NormalForm nEuc n l c k a = NormalForm (VectorValuedDifferentialForm nEuc n l c k a)