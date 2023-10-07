{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}

module Formulative.Algebra.Number.Variation where

import Control.Applicative
import Control.Monad
import Formulative.Algebra.Arithmetic
import GHC.Generics
import Prelude hiding (fromInteger)

data Variation a = Variation {oldVar :: a, newVar :: a}
    deriving stock (Show, Eq, Generic, Functor, Foldable, Traversable)
    deriving anyclass (Additive, AdditiveGroup, Multiplicative, Semiring, Ring)

instance Applicative Variation where
    pure x = Variation x x
    liftA2 op (Variation x1 y1) (Variation x2 y2) = Variation (op x1 x2) (op y1 y2)

-- deriving via (FromPrelude1 Variation a) instance (Absolute a) => Num (Variation a)
-- deriving via (FromPrelude1 Variation a) instance (Field a, Eq a, Absolute a, Ring a) => Fractional (Variation a)
-- deriving via (FromPrelude1 Variation a) instance (Eq a, Floating a, Absolute a, Field a) => Floating (Variation a)

-- zipWithのある型に対してのみ適用可能
-- discreteVariation :: (Floating a, Eq a, AdditiveGroup a, Field a) => (forall x. Floating x => x -> x) -> Variation a -> a
-- discreteVariation f (Variation x y) = if (y - x) == zero then diff f x else (f y - f x) / (y - x)

-- symmetrizeFunc f (x0) (x0') =  1 / 2 * ( f (x0) + f (x0') )
-- symmetrizeFunc f (x0,x1) (x0',x1') =  1 / 4 * ( f (x0,x1) + f (x0,x1') + f (x0',x1) + f (x0',x1') )
-- symmetrizeFunc f a b = symmetrizeFunc' f 0 (h a b) zero
--   where
--     symmetrizeFunc' f i (x : l) a = symmetrizeFunc' f (i + 1) l (f x + a)
--     symmetrizeFunc' f i [] a = (recip . fromInteger $ i) *< a
--     g a b = [a, b]
--     h a b = sequenceA (zipWithV g a b)

-- f a b = Variation a b

-- g = zipWith f

-- >>> g [0,1] [2,3]
-- [[0,2],[1,3]]

-- sequence [[True, False],[True,False],[True,False]]
-- [[True,True,True],[True,True,False],[True,False,True],[True,False,False],[False,True,True],[False,True,False],[False,False,True],[False,False,False]]