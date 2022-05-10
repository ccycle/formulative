{-# LANGUAGE AllowAmbiguousTypes #-}

module Formulative.Calculation.Optimization.Constrained.Effect where

import Control.Algebra
import Data.Kind
import Formulative.Calculation.Optimization.Constrained.AugmentedLagrangian
import Formulative.Calculation.VectorSpace.Class

-- Effect
data ConstrainedSystem a (m :: Type -> Type) k where
    AskConstrainedSystemParameter :: ConstrainedSystem a m (ConstrainedSystemSetting (Scalar a))
    GetLagrangeMultiplier :: ConstrainedSystem a m (LagrangeMultiplier a)
    PutLagrangeMultiplier :: LagrangeMultiplier a -> ConstrainedSystem a m ()

askConstrainedSystemParameter :: forall a sig m. (Has (ConstrainedSystem a) sig m) => m (ConstrainedSystemSetting (Scalar a))
askConstrainedSystemParameter = send (AskConstrainedSystemParameter @a)

askAugmentedLagrangianParameter :: forall a sig m. (Has (ConstrainedSystem a) sig m) => m (AugmentedLagrangianMethodParameters (Scalar a))
askAugmentedLagrangianParameter = augmentedLagrangian <$> (askConstrainedSystemParameter @a)

askTorelance :: forall a sig m. (Has (ConstrainedSystem a) sig m) => m (TorelanceForConstrainedCondition (Scalar a))
askTorelance = torelance <$> (askConstrainedSystemParameter @a)

getLagrangeMultiplier :: (Has (ConstrainedSystem a) sig m) => m a
getLagrangeMultiplier = unLagrangeMultiplier <$> send GetLagrangeMultiplier
putLagrangeMultiplier :: forall a sig m. (Has (ConstrainedSystem a) sig m) => LagrangeMultiplier a -> m ()
putLagrangeMultiplier x = send (PutLagrangeMultiplier x)