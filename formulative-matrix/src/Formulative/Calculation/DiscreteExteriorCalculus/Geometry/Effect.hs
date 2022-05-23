module Formulative.Calculation.DiscreteExteriorCalculus.Geometry.Effect where

import Control.Algebra
import Data.Kind
import Data.Proxy
import Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Types
import Formulative.Calculation.DiscreteExteriorCalculus.Geometry.Types
import Formulative.Calculation.DiscreteExteriorCalculus.Homology.Types
import GHC.TypeNats

-- Has (Operator ExteriorDerivativeType n l c k a) sig m => ...
-- runGeometry geometricData ...

-- data Metric (n :: Dim) l a m k where
--   GetMetric :: Proxy k -> SCellType c -> Metric n l a m (SizedVector nEuc (SizedVector (ToMatSize nBase l c k) a))

data PointData (nEuc :: EucDim) (p :: Nat) a (m :: Type -> Type) k where
    GetPointData :: PointData nEuc p a m (AllPointDataPrimal0 nEuc p a)
getPointData :: (Has (PointData n p a) sig m) => m (AllPointDataPrimal0 n p a)
getPointData = send GetPointData

data SomePointData a m k = forall nEuc p. SomePointData (PointData nEuc p a m k)