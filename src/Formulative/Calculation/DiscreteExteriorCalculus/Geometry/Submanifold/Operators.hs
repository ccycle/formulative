module Formulative.Calculation.DiscreteExteriorCalculus.Geometry.Submanifold.Operators where

import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Operators.Geometry
import Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Types
import Formulative.Calculation.DiscreteExteriorCalculus.Geometry.Submanifold.Types
import Formulative.Calculation.VectorSpace.InnerProductSpace

normalForm :: NormalForm nEuc n l Dual 0 a
normalForm = undefined

volumeClosedManifold (PositionForm x) (NormalForm n) = do
    h <- hodgeStar
    return $ x <.> h n
