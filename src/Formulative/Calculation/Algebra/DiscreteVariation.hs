{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}

module Formulative.Calculation.Algebra.DiscreteVariation where

import Control.Applicative
import Control.Monad
import Control.Monad.ST.Strict
import Data.STRef.Strict
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Algebra.DualNumber
import Formulative.Calculation.Internal.DerivingPrelude
import Formulative.Calculation.Internal.List
import Formulative.Calculation.Internal.Types
import Formulative.Calculation.VectorSpace.Class
import GHC.Generics
import Prelude hiding (fromInteger)

data DiscreteVariable a = DiscreteVariable {getOldVar :: a, getNewVar :: a}
    deriving stock (Show, Eq, Generic, Functor, Foldable, Traversable)
    deriving anyclass (Additive, AdditiveGroup, Multiplicative, Semiring, Ring, Field)

instance Applicative DiscreteVariable where
    pure x = DiscreteVariable x x
    liftA2 op (DiscreteVariable x1 y1) (DiscreteVariable x2 y2) = DiscreteVariable (op x1 x2) (op y1 y2)

deriving via (MyApplicative DiscreteVariable a) instance (Absolute a) => Num (DiscreteVariable a)
deriving via (MyApplicative DiscreteVariable a) instance (Field a, Eq a, Absolute a, Ring a) => Fractional (DiscreteVariable a)
deriving via (MyApplicative DiscreteVariable a) instance (Eq a, Floating a, Absolute a, Field a) => Floating (DiscreteVariable a)

-- zipWithのある型に対してのみ適用可能
discreteVariation :: (Floating a, Eq a, AdditiveGroup a, Field a) => (forall x. Floating x => x -> x) -> DiscreteVariable a -> a
discreteVariation f (DiscreteVariable x y) = if (y .-. x) == zero then diff f x else (f y .-. f x) ./. (y .-. x)

-- symmetrizeFunc f (x0) (x0') =  1 / 2 * ( f (x0) + f (x0') )
-- symmetrizeFunc f (x0,x1) (x0',x1') =  1 / 4 * ( f (x0,x1) + f (x0,x1') + f (x0',x1) + f (x0',x1') )
symmetrizeFunc f a b = symmetrizeFunc' f 0 (h a b) zero
  where
    symmetrizeFunc' f i (x : l) a = symmetrizeFunc' f (i + 1) l (f x .+. a)
    symmetrizeFunc' f i [] a = (reciprocal . fromInteger $ i) *. a
    g a b = [a, b]
    h a b = sequenceA (zipWithV g a b)

symmetrizePoly n a b = runST $ do
    x0 <- newSTRef zero
    forM_ [0 .. n] $ \i -> do
        modifySTRef x0 (.+. (a .^ i .*. b .^ (n - i)))
    modifySTRef x0 (./ fromInteger (n + 1))
    readSTRef x0

-- f a b = DiscreteVariable a b

-- g = zipWith f

-- >>> g [0,1] [2,3]
-- [[0,2],[1,3]]

-- sequence [[True, False],[True,False],[True,False]]
-- [[True,True,True],[True,True,False],[True,False,True],[True,False,False],[False,True,True],[False,True,False],[False,False,True],[False,False,False]]