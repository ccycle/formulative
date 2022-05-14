module Formulative.Calculation.Optimization.Constrained.Class where

import Formulative.Calculation.Optimization.Constrained.Types

class HasEqualityConstraintM m a where
    type EqualityConstraintType a :: *
    getEqualityConstraintM :: m (a -> EqualityConstraintType a)
class HasGradPenaltyM m a where
    getGradPenaltyM :: m (LagrangeMultiplier (EqualityConstraintType a) -> a -> a)