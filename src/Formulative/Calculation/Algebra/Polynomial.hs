module Formulative.Calculation.Algebra.Polynomial where

import Control.Monad.ST

import Data.Data (Data, Typeable)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M

import qualified Numeric.IEEE as IEEE

data QuadParam a = QuadParam
    { -- | Relative precision of answer
      quadPrecision :: a
    , -- | Maximum number of iterations
      quadMaxIter :: Int
    }
    deriving stock (Show, Eq, Data, Typeable)

-- Number of iterations limited to 30
maxIter :: QuadParam a -> Int
maxIter = min 30 . quadMaxIter

{- | Default parameters for integration functions

 * Maximum number of iterations = 20

 * Precision is 10&#8315;&#8313;
-}
defQuad :: Floating a => QuadParam a
defQuad =
    QuadParam
        { quadPrecision = 1e-9
        , quadMaxIter = 20
        }

-- | Result of numeric integration.
data QuadRes a = QuadRes
    { -- | Integraion result
      quadRes :: Maybe a
    , -- | Best estimate of integral
      quadBestEst :: a
    , -- | Rough estimate of attained precision
      quadPrecEst :: a
    , -- | Number of iterations
      quadNIter :: Int
    }
    deriving (Show, Eq, Data, Typeable)

----------------------------------------------------------------
-- Different integration methods
----------------------------------------------------------------

{- | Integration of using trapezoids. This is robust algorithm and
   place and useful for not very smooth. But it is very slow. It
   hundreds times slower then 'quadRomberg' if function is
   sufficiently smooth.
-}
quadTrapezoid ::
    (Floating a, M.Unbox a, IEEE.IEEE a) =>
    -- | Parameters
    QuadParam a ->
    -- | Integration limits
    (a, a) ->
    -- | Function to integrate
    (a -> a) ->
    QuadRes a
quadTrapezoid param (a, b) f = worker 1 1 (trapGuess a b f)
  where
    eps = quadPrecision param -- Requred precision
    maxN = maxIter param -- Maximum allowed number of iterations
    worker n nPoints q
        | n > 5 && converged eps q q' = done True
        | n >= maxN = done False
        | otherwise = worker (n + 1) (nPoints * 2) q'
      where
        q' = nextTrapezoid a b nPoints f q -- New approximation
        done True = QuadRes (Just q') q' (estimatePrec q q') n
        done False = QuadRes Nothing q' (estimatePrec q q') n

{- | Integration using Simpson rule. It should be more efficient than
   'quadTrapezoid' if function being integrated have finite fourth
   derivative.
-}
quadSimpson ::
    (Floating a, IEEE.IEEE a, M.Unbox a) =>
    -- | Parameters
    QuadParam a ->
    -- | Integration limits
    (a, a) ->
    -- | Function to integrate
    (a -> a) ->
    QuadRes a
quadSimpson param (a, b) f = worker 1 1 0 (trapGuess a b f)
  where
    eps = quadPrecision param -- Requred precision
    maxN = maxIter param -- Maximum allowed number of points for evaluation
    worker n nPoints s st
        | n > 5 && converged eps s s' = done True
        | n >= maxN = done False
        | otherwise = worker (n + 1) (nPoints * 2) s' st'
      where
        st' = nextTrapezoid a b nPoints f st
        s' = (4 * st' - st) / 3
        done True = QuadRes (Just s') s' (estimatePrec s s') n
        done False = QuadRes Nothing s' (estimatePrec s s') n

{- | Integration using Romberg rule. For sufficiently smooth functions
   (e.g. analytic) it's a fastest of three.
-}
quadRomberg ::
    (Floating a, M.Unbox a, IEEE.IEEE a) =>
    -- | Parameters
    QuadParam a ->
    -- | Integration limits
    (a, a) ->
    -- | Function to integrate
    (a -> a) ->
    QuadRes a
quadRomberg param (a, b) f =
    runST $ do
        let eps = quadPrecision param
            maxN = maxIter param
        arr <- M.new (maxN + 1)
        -- Calculate new approximation
        let nextAppr n = runNextAppr 0 4
              where
                runNextAppr i fac s = do
                    x <- M.read arr i
                    M.write arr i s
                    if i >= n
                        then return s
                        else runNextAppr (i + 1) (fac * 4) $ s + (s - x) / (fac - 1)
        -- Maine loop
        let worker n nPoints st s = do
                let st' = nextTrapezoid a b nPoints f st
                s' <- M.write arr 0 st >> nextAppr n st'
                let done True = return $ QuadRes (Just s') s' (estimatePrec s s') n
                    done False = return $ QuadRes Nothing s' (estimatePrec s s') n
                case () of
                    _
                        | n > 5 && converged eps s s' -> done True
                        | n >= maxN -> done False
                        | otherwise -> worker (n + 1) (nPoints * 2) st' s'
        -- Calculate integral
        worker 1 1 st0 st0
  where
    st0 = trapGuess a b f

----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- Initial guess for trapezoid rule
trapGuess :: Floating a => a -> a -> (a -> a) -> a
trapGuess !a !b f = 0.5 * (b - a) * (f b + f a)

-- Refinement of guess using trapeziod algorithms
nextTrapezoid ::
    (Floating a, M.Unbox a) =>
    a -> -- Lower integration limit
    a -> -- Upper integration limit
    Int -> -- Number of additional points
    (a -> a) -> -- Function to integrate
    a -> -- Approximation
    a
nextTrapezoid !a !b !n f !q = 0.5 * (q + sep * s)
  where
    sep = (b - a) / fromIntegral n -- Separation between points
    x0 = a + 0.5 * sep -- Starting point
    s = U.sum $ U.map f $ U.iterateN n (+ sep) x0 -- Sum of all points

-- Check for convergence. Convergence to zero is tricky case since we
-- use relative error and checking for convergence to zero requires
-- absolute one. Instead iteration have converged if two successive
-- operations produced zero
converged :: (Floating a, Eq a, IEEE.IEEE a) => a -> a -> a -> Bool
converged eps q q'
    -- Iterations yielded same numbers.
    | q == q' = True
    -- Both numbers have identical IEEE representation It's not the same
    -- as previous clause because of excess precision. And previous
    -- clause is required because of negative zero.
    | IEEE.identicalIEEE q q' = True
    -- Usual check for convergence.
    | otherwise = abs (q' - q) < eps * abs q
{-# INLINE converged #-}

-- Estimate precision
estimatePrec :: (Floating a, Eq a) => a -> a -> a
estimatePrec q q'
    | q == 0 && q' == 0 = 0
    | otherwise = abs (q - q') / abs q