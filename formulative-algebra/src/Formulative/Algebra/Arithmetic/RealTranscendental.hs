module Formulative.Algebra.Arithmetic.RealTranscendental where

import Formulative.Algebra.Arithmetic.Field
import Formulative.Algebra.Arithmetic.Transcendental
import Formulative.Algebra.Internal.FromPrelude
import Prelude hiding (atan2)
import qualified Prelude as P

class (Transcendental a) => RealTranscendental a where
    atan2 :: a -> a -> a

instance (P.RealFloat a, Field a) => RealTranscendental (FromPrelude a) where
    atan2 (FromPrelude x) (FromPrelude y) = FromPrelude (P.atan2 x y)

deriving via (FromPrelude Double) instance RealTranscendental Double
deriving via (FromPrelude Float) instance RealTranscendental Float