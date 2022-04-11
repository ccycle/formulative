module Formulative.Calculation.DiscreteExteriorCalculus.Homology.Effect where

import Control.Algebra
import Data.Kind
import Formulative.Calculation.DiscreteExteriorCalculus.Homology.Types
import GHC.TypeNats

data Connectivity (n :: Nat) (l :: SSizes) (m :: Type -> Type) k where
    GetPrimitiveSimplicies :: Connectivity n l m (Simplices n l)
getPrimitiveSimplicies :: (Has (Connectivity n l) sig m) => m (Simplices n l)
getPrimitiveSimplicies = send GetPrimitiveSimplicies