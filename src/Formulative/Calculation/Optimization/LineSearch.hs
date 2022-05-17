{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Formulative.Calculation.Optimization.LineSearch where

import Data.Coerce
import Data.Hashable
import qualified Data.Vector as V
import Dhall
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.VectorSpace.Class
import Formulative.Preprocess.DefaultValue

newtype Residuals a = Residuals (V.Vector a)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromDhall, ToDhall)

-- https://en.wikipedia.org/wiki/Wolfeconditions#Armijorule_and_curvature
newtype ArmijoConditionParameter a = ArmijoConditionParameter a
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Enum, Fractional)
  deriving anyclass (FromDhall, ToDhall, Hashable)
instance (Fractional a) => HasDefaultValue (ArmijoConditionParameter a) where
  defaultValue = 1e-4

-- https://en.wikipedia.org/wiki/Wolfeconditions#Armijorule_and_curvature
newtype WolfeConditionParameter a = WolfeConditionParameter a
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromDhall, ToDhall, Hashable)
instance (Fractional a) => HasDefaultValue (WolfeConditionParameter a) where
  defaultValue = WolfeConditionParameter 0.9

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
instance (Fractional a) => HasDefaultValue (InequalityConditionForLineSearch a) where
  defaultValue = Armijo (ArmijoConditionParameter 1.0e-4)

-- step size for ine search
newtype StepSizeForLineSearch a = StepSizeForLineSearch a
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Fractional)
  deriving anyclass (FromDhall, ToDhall, Hashable)
unStepSizeForLineSearch :: StepSizeForLineSearch a -> a
unStepSizeForLineSearch = coerce
instance (Fractional a) => HasDefaultValue (StepSizeForLineSearch a) where
  defaultValue = 1.0

-- backtracking method
newtype BacktrackingFactor a = BacktrackingFactor a
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Num, Fractional)
  deriving anyclass (FromDhall, ToDhall, Hashable)
instance (Fractional a) => HasDefaultValue (BacktrackingFactor a) where
  defaultValue = 1.0

newtype IterationNumberForLineSearch = IterationNumberForLineSearch Natural
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (Enum, Num)
  deriving anyclass (FromDhall, ToDhall, Hashable, HasDefaultValue)

data BacktrackingParameter a = BacktrackingParameter
  { factor :: BacktrackingFactor a
  , maximumIterationNumber :: IterationNumberForLineSearch
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromDhall, ToDhall, Hashable, HasDefaultValue)

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
instance (Fractional a) => HasDefaultValue (LineSearchParameters a) where
  defaultValue = FixedStepSize defaultValue

newtype ObjectiveFunction a = ObjectiveFunction (a -> Scalar a)
newtype GradObjectiveFunction a = GradObjectiveFunction (a -> a)

-- must be (0 < ArmijoConditionParameter < WolfeConditionParameter < 1)

armijoCondition (ArmijoConditionParameter c1) (StepSizeForLineSearch alpha) x (DescentDirection p) (ObjectiveFunction f) (GradObjectiveFunction gradf) = lhs <= rhs
 where
  lhs = f (x .+. (alpha *. p))
  rhs = f x .+. c1 .*. alpha .*. (gradf x <.> p)

wolfeCondition (WolfeConditionParameter c2) (StepSizeForLineSearch alpha) x (DescentDirection p) (GradObjectiveFunction gradf) = lhs <= rhs
 where
  lhs = c2 .*. p <.> gradf x
  rhs = p <.> gradf (x .+. (alpha *. p))

strongWolfeCondition (WolfeConditionParameter c2) (StepSizeForLineSearch alpha) x (DescentDirection p) (GradObjectiveFunction gradf) = lhs <= rhs
 where
  rhs = abs' $ p <.> gradf x
  lhs = c2 .*. abs' (p <.> gradf (x .+. (alpha *. p)))

lineSearchCondition (Armijo a) alpha x p f gradf = armijoCondition a alpha x p f gradf
lineSearchCondition (Wolfe a w) alpha x p f gradf = armijoCondition a alpha x p f gradf && wolfeCondition w alpha x p gradf
lineSearchCondition (StrongWolfe a w) alpha x p f gradf = armijoCondition a alpha x p f gradf && strongWolfeCondition w alpha x p gradf

newtype DescentDirection a = DescentDirection a
  deriving stock (Show)
unDescentDirection :: DescentDirection a -> a
unDescentDirection = coerce

-- data BacktrackingException a b c d = BacktrackingException a b c d deriving (Show)
-- instance (Show a, Typeable a, Show b, Typeable b, Show c, Typeable c, Show d, Typeable d) => Exception (BacktrackingException a b c d)

-- https://en.wikipedia.org/wiki/Backtracking_line_search#Algorithm
-- tau: BacktrackingFactor
-- TODO: loggerの作成(1回のiterationに対し値がどのように変化しているかを追跡できるように)
backtrackingLineSearch ineqConds (StepSizeForLineSearch alpha) (BacktrackingFactor tau) i x p f gradf =
  if i == 0 || lineSearchCondition ineqConds (StepSizeForLineSearch alpha) x p f gradf
    then StepSizeForLineSearch alpha
    else backtrackingLineSearch ineqConds (StepSizeForLineSearch (tau .*. alpha)) (BacktrackingFactor tau) (pred i) x p f gradf

getStepSizeFromLineSearch (FixedStepSize a) _ _ _ _ = a
getStepSizeFromLineSearch Backtracking{..} x p f gradf = backtrackingLineSearch inequality initialStepSize factor maximumIterationNumber x p f gradf