{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}

module Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Operators (
    module Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Operators.Geometry,
    module Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Operators.Homology,
) where

import Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Operators.Geometry
import Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Operators.Homology
