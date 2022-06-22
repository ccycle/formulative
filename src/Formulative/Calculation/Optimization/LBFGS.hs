{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Formulative.Calculation.Optimization.LBFGS where

import Control.Exception.Safe
import Control.Monad
import Control.Monad.ST.Strict
import Data.Hashable
import Data.Maybe
import Data.STRef.Strict
import qualified Data.Vector as V
import Dhall
import Formulative.Calculation.Algebra.Arithmetic
import Formulative.Calculation.Internal.List
import Formulative.Calculation.Optimization.LineSearch
import Formulative.Calculation.VectorSpace.Class
import Formulative.Preprocess.DefaultValue
import Formulative.Preprocess.Exception
import GHC.Exts (fromList)

-- paremeters
-- TODO: machine epsilon以下に指定したら警告を出すようにする
-- NOTE: Numeric-Limitsを使う
-- https://hackage.haskell.org/package/numeric-limits-0.1.0.0/docs/Numeric-Limits.html
newtype NumericalError a = NumericalError a
    deriving stock (Generic, Show, Eq)
    deriving newtype (Enum, Num, Fractional)
    deriving anyclass (FromDhall, ToDhall, Hashable)
instance (Fractional a) => HasDefaultValue (NumericalError a) where
    defaultValue = 1e-8
data ConvergenceTestType = AbsoluteError | RelativeError
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable)
instance HasDefaultValue ConvergenceTestType where
    defaultValue = RelativeError
data ConvergenceTestParameters a = ConvergenceTestParameters {numericalError :: NumericalError a, convergenceTestType :: ConvergenceTestType, normType :: NormType a}
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable, HasDefaultValue)

-- convergence test
-- Absolute: |x-x0|<epsilon_A
-- Relative: \frac{|x-x0|}{|x|}<epsilon_R
convergenceTestFunc (ConvergenceTestParameters ((NumericalError epA)) AbsoluteError t) x0 x = norm t (x .-. x0) < epA
convergenceTestFunc (ConvergenceTestParameters ((NumericalError epR)) RelativeError t) x0 x = norm t (x .-. x0) < epR .*. norm t x

newtype MaxIterationLBFGS = MaxIterationLBFGS Natural
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (Enum, Num)
    deriving anyclass (FromDhall, ToDhall, Hashable)
instance HasDefaultValue MaxIterationLBFGS where
    defaultValue = 10000
newtype HistorySize = HistorySize Natural
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (Num)
    deriving anyclass (FromDhall, ToDhall, Hashable)
instance HasDefaultValue HistorySize where
    defaultValue = 10
data LBFGSParameters = LBFGSParameters {historySize :: HistorySize, maximumIterationNumber :: MaxIterationLBFGS}
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable, HasDefaultValue)

initN list = fromMaybe [] (initMaybe list)

searchDirectionLBFGS x (GradObjectiveFunction gradf) ysList = DescentDirection $ if descentTest gradf x d then d else negation d
  where
    rho (y, s) = reciprocal $ y <.> s
    alpha (y, s) q = rho (y, s) .*. s <.> q
    gamma (y, s) = y <.> s ./. y <.> y
    beta (y, s) z = rho (y, s) .*. y <.> z
    descentTest gradf x d = (d <.> gradf x) < zero
    d = runST $ do
        let gradfx = gradf x
        discentDirection <- newSTRef gradfx
        alphaList <- newSTRef []
        forM_ ysList $ \(y, s) -> do
            discentDirection' <- readSTRef discentDirection
            modifySTRef alphaList (++ [alpha (y, s) discentDirection'])
            modifySTRef discentDirection (\q -> q .-. (alpha (y, s) q *. y))
        case headMaybe ysList of
            Just ysList' -> modifySTRef discentDirection (\q -> gamma ysList' *. q)
            Nothing -> return ()
        alphaList' <- readSTRef alphaList
        let ysaList = zip ysList alphaList'
        forM_ (reverse ysaList) $ \((y, s), a) ->
            modifySTRef discentDirection (\z -> z .+. ((a .-. beta (y, s) z) *. s))
        modifySTRef discentDirection negation
        readSTRef discentDirection

updateYsList xOld xNew (GradObjectiveFunction gradF) ysList (HistorySize historySize) =
    if length ysList <= fromIntegral historySize
        then (gradF xNew .-. gradF xOld, xNew .-. xOld) : ysList
        else (gradF xNew .-. gradF xOld, xNew .-. xOld) : initN ysList

updateLBFGS lineSearchParam x (ObjectiveFunction f) (GradObjectiveFunction gradf) ysList =
    (x .+. (unStepSizeForLineSearch alpha' *. unDescentDirection p), alpha', p)
  where
    p = searchDirectionLBFGS x (GradObjectiveFunction gradf) ysList
    alpha' = getStepSizeFromLineSearch lineSearchParam x p (ObjectiveFunction f) (GradObjectiveFunction gradf)

lbfgsMethod lineSearchParam convergenceTestParam lbfgsParam f gradf =
    go
        lineSearchParam
        convergenceTestParam
        lbfgsParam
        f
        gradf
        []
  where
    go
        lineSearchParam
        convergenceParam
        lbfgsParam@(LBFGSParameters historySize k)
        (ObjectiveFunction f)
        (GradObjectiveFunction gradf)
        ysList
        x =
            do
                let gradfx = gradf x
                if k == 0
                    then
                        throw $
                            ConvergenceException
                                (norm (normType convergenceParam) $ gradf x)
                    else do
                        let (xNew, alphaNew, p) = updateLBFGS lineSearchParam x (ObjectiveFunction f) (GradObjectiveFunction gradf) ysList
                        let gradfxNew = gradf xNew
                        if convergenceTestFunc convergenceParam x xNew
                            then return x
                            else
                                go
                                    lineSearchParam
                                    convergenceParam
                                    (LBFGSParameters historySize (pred k))
                                    (ObjectiveFunction f)
                                    (GradObjectiveFunction gradf)
                                    (updateYsList x xNew (GradObjectiveFunction gradf) ysList historySize)
                                    xNew