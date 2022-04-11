{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Formulative.Calculation.Optimization.LineSearch where

import Control.Exception.Safe
import Data.Coerce
import Data.Hashable
import qualified Data.Vector as V
import Dhall
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.VectorSpace.Class
import Formulative.Preprocess.DefaultValue

newtype Residuals a = MkResiduals (V.Vector a)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromDhall, ToDhall)

-- line search consitions
-- data LineSearchConditionType = Armijo | Wolfe | StrongWolfe
--   deriving stock (Generic, Show, Eq)
--   deriving anyclass (FromDhall, ToDhall, Hashable)
-- instance DefaultValue LineSearchConditionType where
--   defaultValue = Armijo

-- https://en.wikipedia.org/wiki/Wolfeconditions#Armijorule_and_curvature
newtype ArmijoConditionParameter a = MkArmijoConditionParameter a
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Enum)
  deriving anyclass (FromDhall, ToDhall, Hashable)
instance (Fractional a) => DefaultValue (ArmijoConditionParameter a) where
  defaultValue = MkArmijoConditionParameter 1e-4

-- https://en.wikipedia.org/wiki/Wolfeconditions#Armijorule_and_curvature
newtype WolfeConditionParameter a = MkWolfeConditionParameter a
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromDhall, ToDhall, Hashable)
instance (Fractional a) => DefaultValue (WolfeConditionParameter a) where
  defaultValue = MkWolfeConditionParameter 0.9

-- TODO: condition typeを修正
-- data InequalityConditionForLineSearch a = InequalityConditionForLineSearch
--   { conditionType :: LineSearchConditionType
--   , armijoConditionParameter :: ArmijoConditionParameter a
--   , wolfeConditionParameter :: WolfeConditionParameter a
--   }
--   deriving stock (Generic, Show, Eq)
--   deriving anyclass (FromDhall, ToDhall, Hashable, DefaultValue)
data InequalityConditionForLineSearch a
  = Armijo
      {c1 :: ArmijoConditionParameter a}
  | Wolfe
      { c1 :: ArmijoConditionParameter a
      , c2 :: WolfeConditionParameter a
      }
  | StrongWolfe
      { c1 :: ArmijoConditionParameter a
      , c2 :: WolfeConditionParameter a
      }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromDhall, ToDhall, Hashable)
instance (Fractional a) => DefaultValue (InequalityConditionForLineSearch a) where
  defaultValue = Armijo (MkArmijoConditionParameter 1.0e-4)

-- step size for ine search
newtype StepSizeForLineSearch a = MkStepSizeForLineSearch a
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num)
  deriving anyclass (FromDhall, ToDhall, Hashable)
unMkStepSizeForLineSearch :: StepSizeForLineSearch a -> a
unMkStepSizeForLineSearch = coerce
instance (Fractional a) => DefaultValue (StepSizeForLineSearch a) where
  defaultValue = MkStepSizeForLineSearch 1.0

-- backtracking method
newtype BacktrackingFactor a = MkBacktrackingFactor a
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num)
  deriving anyclass (FromDhall, ToDhall, Hashable)
instance (Fractional a) => DefaultValue (BacktrackingFactor a) where
  defaultValue = MkBacktrackingFactor 1.0

newtype IterationNumberForLineSearch = MkIterationNumberForLineSearch Natural
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Enum, Num)
  deriving anyclass (FromDhall, ToDhall, Hashable)
instance DefaultValue IterationNumberForLineSearch where
  defaultValue = 0

data BacktrackingParameter a = MkBacktrackingParameter
  { factor :: BacktrackingFactor a
  , maximumIterationNumber :: IterationNumberForLineSearch
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromDhall, ToDhall, Hashable, DefaultValue)

-- line search parameter
-- data LineSearchParameters a = MkLineSearchParameters
--   { inequality :: InequalityConditionForLineSearch a
--   , maximumStepSize :: StepSizeForLineSearch a
--   , backtracking :: BacktrackingParameter a
--   }
--   deriving stock (Generic, Show, Eq)
--   deriving anyclass (FromDhall, ToDhall, Hashable, DefaultValue)
data LineSearchParameters a
  = FixedStepSize {stepSize :: StepSizeForLineSearch a}
  | Backtracking
      { inequality :: InequalityConditionForLineSearch a
      , factor :: BacktrackingFactor a
      , initialStepSize :: StepSizeForLineSearch a
      , maximumIterationNumber :: IterationNumberForLineSearch
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromDhall, ToDhall, Hashable)
instance (Fractional a) => DefaultValue (LineSearchParameters a) where
  defaultValue = FixedStepSize defaultValue

newtype ObjectiveFunction a = MkObjectiveFunction (a -> Scalar a)
newtype GradObjectiveFunction a = MkGradObjectiveFunction (a -> a)

-- must be (0 < ArmijoConditionParameter < WolfeConditionParameter < 1)

armijoCondition (MkArmijoConditionParameter c1) (MkStepSizeForLineSearch alpha) x (MkDescentDirection p) (MkObjectiveFunction f) (MkGradObjectiveFunction gradf) = lhs <= rhs
 where
  lhs = f (x .+. (alpha *. p))
  rhs = f x .+. c1 .*. alpha .*. (gradf x <.> p)

wolfeCondition (MkWolfeConditionParameter c2) (MkStepSizeForLineSearch alpha) x (MkDescentDirection p) (MkGradObjectiveFunction gradf) = lhs <= rhs
 where
  lhs = c2 .*. p <.> gradf x
  rhs = p <.> gradf (x .+. (alpha *. p))

strongWolfeCondition (MkWolfeConditionParameter c2) (MkStepSizeForLineSearch alpha) x (MkDescentDirection p) (MkGradObjectiveFunction gradf) = lhs <= rhs
 where
  rhs = abs' $ p <.> gradf x
  lhs = c2 .*. abs' (p <.> gradf (x .+. (alpha *. p)))

lineSearchCondition (Armijo a) alpha x p f gradf = armijoCondition a alpha x p f gradf
lineSearchCondition (Wolfe a w) alpha x p f gradf = armijoCondition a alpha x p f gradf && wolfeCondition w alpha x p gradf
lineSearchCondition (StrongWolfe a w) alpha x p f gradf = armijoCondition a alpha x p f gradf && strongWolfeCondition w alpha x p gradf

newtype DescentDirection a = MkDescentDirection a
  deriving stock (Show)
unMkDescentDirection :: DescentDirection a -> a
unMkDescentDirection = coerce

-- data BacktrackingException a b c d = BacktrackingException a b c d deriving (Show)
-- instance (Show a, Typeable a, Show b, Typeable b, Show c, Typeable c, Show d, Typeable d) => Exception (BacktrackingException a b c d)

-- https://en.wikipedia.org/wiki/Backtracking_line_search#Algorithm
-- tau: BacktrackingFactor
-- TODO: make logger function
backtrackingLineSearch ineqConds (MkStepSizeForLineSearch alpha) (MkBacktrackingFactor tau) i x p f gradf =
  if i == 0 || lineSearchCondition ineqConds (MkStepSizeForLineSearch alpha) x p f gradf
    then MkStepSizeForLineSearch alpha
    else backtrackingLineSearch ineqConds (MkStepSizeForLineSearch (tau .*. alpha)) (MkBacktrackingFactor tau) (pred i) x p f gradf

lineSearch (FixedStepSize a) _ _ _ _ = a
lineSearch Backtracking{..} x p f gradf = backtrackingLineSearch inequality initialStepSize factor maximumIterationNumber x p f gradf