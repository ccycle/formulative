module Formulative.Algebra.Prelude (
    module X,
    IfThenElse (..),
) where

import Data.String as X (fromString)
import Formulative.Algebra.Arithmetic as X
import Formulative.Algebra.Literal.FromInteger as X
import GHC.Natural as X
import Prelude as X hiding (
    Floating (..),
    Fractional (..),
    Num (..),
    Real (..),
    RealFloat (..),
    RealFrac (..),
    fromInteger,
    (^),
 )

class IfThenElse b where
    ifThenElse :: b -> a -> a -> a

instance IfThenElse Bool where
    ifThenElse True t _ = t
    ifThenElse False _ f = f
