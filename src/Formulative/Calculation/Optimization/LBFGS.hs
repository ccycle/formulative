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
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Optimization.LineSearch
import Formulative.Calculation.VectorSpace.Class
import Formulative.Preprocess.DefaultValue
import Formulative.Preprocess.Exception
import GHC.Exts (fromList)
import RIO.List

-- paremeters
-- TODO: machine epsilon以下に指定しないように警告を出すようにする
-- NOTE: Numeric-Limitsを使う
-- https://hackage.haskell.org/package/numeric-limits-0.1.0.0/docs/Numeric-Limits.html
newtype NumericalError a = MkNumericalError a
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable)
instance (Fractional a) => DefaultValue (NumericalError a) where
    defaultValue = MkNumericalError 1e-8
data ConvergenceTestType = AbsoluteError | RelativeError
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable)
instance DefaultValue ConvergenceTestType where
    defaultValue = RelativeError
data ConvergenceTestParameters a = MkConvergenceTestParameters {numericalError :: NumericalError a, convergenceTestType :: ConvergenceTestType, normType :: NormType a}
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable, DefaultValue)

-- convergence test
-- Absolute: |x-x0|<epsilon_A
-- Relative: \frac{|x-x0|}{|x|}<epsilon_R
convergenceTestFunc (MkConvergenceTestParameters ((MkNumericalError epA)) AbsoluteError t) x0 x = norm t (x .-. x0) < epA
convergenceTestFunc (MkConvergenceTestParameters ((MkNumericalError epR)) RelativeError t) x0 x = norm t (x .-. x0) < epR .*. norm t x

newtype MaxIterationLBFGS = MkMaxIterationLBFGS Natural
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (Enum, Num)
    deriving anyclass (FromDhall, ToDhall, Hashable)
instance DefaultValue MaxIterationLBFGS where
    defaultValue = 10000
newtype HistorySize = MkHistorySize Natural
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (Num)
    deriving anyclass (FromDhall, ToDhall, Hashable)
instance DefaultValue HistorySize where
    defaultValue = 10
data LBFGSParameters = MkLBFGSParameters {historySize :: HistorySize, maximumIterationNumber :: MaxIterationLBFGS}
    deriving stock (Generic, Show, Eq)
    deriving anyclass (FromDhall, ToDhall, Hashable, DefaultValue)

initN list = fromMaybe [] (initMaybe list)

searchDirectionLBFGS x (MkGradObjectiveFunction gradf) ysList = MkDescentDirection $ if descentTest gradf x d then d else negation d
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

updateYsList xOld xNew (MkGradObjectiveFunction gradF) ysList (MkHistorySize historySize) =
    if length ysList <= fromIntegral historySize
        then (gradF xNew .-. gradF xOld, xNew .-. xOld) : ysList
        else (gradF xNew .-. gradF xOld, xNew .-. xOld) : initN ysList

updateLBFGS lineSearchParam x (MkObjectiveFunction f) (MkGradObjectiveFunction gradf) ysList =
    (x .+. (unMkStepSizeForLineSearch alpha' *. unMkDescentDirection p), alpha', p)
  where
    p = searchDirectionLBFGS x (MkGradObjectiveFunction gradf) ysList
    alpha' = lineSearch lineSearchParam x p (MkObjectiveFunction f) (MkGradObjectiveFunction gradf)

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
        lbfgsParam@(MkLBFGSParameters historySize k)
        (MkObjectiveFunction f)
        (MkGradObjectiveFunction gradf)
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
                        let (xNew, alphaNew, p) = updateLBFGS lineSearchParam x (MkObjectiveFunction f) (MkGradObjectiveFunction gradf) ysList
                        let gradfxNew = gradf xNew
                        if convergenceTestFunc convergenceParam x xNew
                            then return x
                            else
                                go
                                    lineSearchParam
                                    convergenceParam
                                    (MkLBFGSParameters historySize (pred k))
                                    (MkObjectiveFunction f)
                                    (MkGradObjectiveFunction gradf)
                                    (updateYsList x xNew (MkGradObjectiveFunction gradf) ysList historySize)
                                    xNew