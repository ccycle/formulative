{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.Optimization.Parameter where

import Data.Hashable
import qualified Data.Text as T
import Dhall
import Formulative.Calculation.Optimization.LBFGS
import Formulative.Calculation.Optimization.LineSearch
import Formulative.Preprocess.DefaultValue

data OptimizationParameters a = OptimizationParameters
    { convergenceTest :: ConvergenceTestParameters a
    , lbfgsParameters :: LBFGSParameters
    , lineSearchParameters :: LineSearchParameters a
    }
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable, HasDefaultValue)