module Formulative.Algebra.Arithmetic.Div where

import Data.Kind (Type)
import Formulative.Algebra.Arithmetic.Mul
import qualified GHC.Real as P ((/))
import Prelude (Bool, Double, Eq (..), Float)

class (Mul b c a) => Div a b c | a b -> c, a c -> b, b c -> a where
    (./.) :: a -> b -> c

type family (./.) (a :: Type -> Type) (b :: Type -> Type) :: Type -> Type

infixl 7 ./.

-- a * b = c -> a = c / b -> c / b * b = c
prop_leftUnit :: (Eq a, Div a b c, Mul c b a) => a -> b -> Bool
prop_leftUnit x y = x ./. y .*. y == x

instance Div Double Double Double where
    (./.) = (P./)
instance Div Float Float Float where
    (./.) = (P./)
