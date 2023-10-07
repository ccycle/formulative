{-# LANGUAGE UndecidableInstances #-}

module Formulative.Algebra.Arithmetic.Mul where

import Data.Kind (Type)
import Prelude (Double, Float)
import qualified Prelude as P

-- Multiplication of Different Types
class Mul a b c | a b -> c, a c -> b, b c -> a where
    (.*.) :: a -> b -> c

type family (.*.) (a :: Type -> Type) (b :: Type -> Type) :: Type -> Type

infixl 7 .*.

instance Mul Double Double Double where
    (.*.) = (P.*)
instance Mul Float Float Float where
    (.*.) = (P.*)
