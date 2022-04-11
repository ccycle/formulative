{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.Calculus.Variation where

import Control.Monad
import Control.Monad.ST.Strict
import Data.STRef.Strict
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.VectorSpace.VectorSpace
import GHC.Natural

class Variation a where
    type VariateTo a :: * -- if F(x) is variational function, then "VariateTo a" corresponds to df/dx
    variation :: (forall x. Floating x => x -> x) -> a -> VariateTo a

-- x^n -> (x^(n+1)-y^(n+1))/(x-y)
discreteGradientOfPolynomial n x y
    | n < 0 = error "*** discreteGradientOfPolynomial: negative input"
    | n == 0 = one
    | n == 1 = (x .+. y) ./ 2
    | otherwise = (*.) (1 ./. (n + 1)) $
        runST $ do
            s <- newSTRef (x .^ n .+. y .^ n)
            forM_ [1 .. (n - 1)] $ \i -> do
                modifySTRef s (.+. (x .^ i .*. y .^ (n - i)))
            readSTRef s

-- instance (Floating a, Eq a, AdditiveGroup a, Field a) => Variation (MyFloating a) where
--     type VariateTo (MyFloating a) = a
--     variation f (MyFloating x) = diff f x

-- deriving via (MyFloating Double) instance Variation Double
-- deriving via (MyFloating Float) instance Variation Float
-- deriving via (MyFloating Expr) instance Variation Expr
