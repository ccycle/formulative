{-# LANGUAGE AllowAmbiguousTypes #-}

module Formulative.Calculation.Optimization.Constrained.Effect where

import Control.Algebra
import Data.Kind
import Formulative.Calculation.Optimization.AugmentedLagrangian
import Formulative.Calculation.VectorSpace.Class

-- Effect
data ConstrainedSystem a (m :: Type -> Type) k where
    AskConstrainedSystemParameter :: ConstrainedSystem a m (ConstrainedSystemParameter (Scalar a))
    GetLagrangianMultiplier :: ConstrainedSystem a m (LagrangianMultiplier a)
    PutLagrangianMultiplier :: LagrangianMultiplier a -> ConstrainedSystem a m ()

askConstrainedSystemParameter :: forall a sig m. (Has (ConstrainedSystem a) sig m) => m (ConstrainedSystemParameter (Scalar a))
askConstrainedSystemParameter = send (AskConstrainedSystemParameter @a)

askAugmentedLagrangianParameter :: forall a sig m. (Has (ConstrainedSystem a) sig m) => m (AugmentedLagrangianMethodParameters (Scalar a))
askAugmentedLagrangianParameter = augmentedLagrangianMethodParameters <$> (askConstrainedSystemParameter @a)

getLagrangianMultiplier :: (Has (ConstrainedSystem a) sig m) => m (LagrangianMultiplier a)
getLagrangianMultiplier = send GetLagrangianMultiplier
putLagrangianMultiplier :: forall a sig m. (Has (ConstrainedSystem a) sig m) => LagrangianMultiplier a -> m ()
putLagrangianMultiplier x = send (PutLagrangianMultiplier x)