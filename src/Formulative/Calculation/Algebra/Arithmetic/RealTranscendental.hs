module Formulative.Calculation.Algebra.Arithmetic.RealTranscendental where

import Formulative.Calculation.Algebra.Arithmetic.Field
import Formulative.Calculation.Algebra.Arithmetic.Transcendental
import Formulative.Calculation.Internal.Types

class (Transcendental a) => RealTranscendental a where
    atan2' :: a -> a -> a

instance (RealFloat a, Field a) => RealTranscendental (MyNumeric a) where
    atan2' (MyNumeric x) (MyNumeric y) = MyNumeric (Prelude.atan2 x y)

deriving via (MyNumeric Double) instance RealTranscendental Double
deriving via (MyNumeric Float) instance RealTranscendental Float