module Formulative.Algebra.Arithmetic.Absolute where

import qualified Data.Vector.Sized as VS
import Formulative.Algebra.Arithmetic.Ring
import Formulative.Algebra.Internal.FromPrelude
import GHC.TypeNats
import Prelude
import qualified Prelude as P

class (Ring a) => Absolute a where
    absolute :: a -> a
    sign :: a -> a

instance (Num a) => Absolute (FromPrelude a) where
    absolute = FromPrelude . P.abs . unMyNumeric
    sign = FromPrelude . P.signum . unMyNumeric

deriving via (FromPrelude Double) instance Absolute Double
deriving via (FromPrelude Float) instance Absolute Float
deriving via (FromPrelude Int) instance Absolute Int
deriving via (FromPrelude Integer) instance Absolute Integer

-- applicative
instance (Absolute a, Applicative m) => Absolute (FromPrelude1 m a) where
    absolute = FromPrelude1 . fmap absolute . unMyApplicative
    sign = FromPrelude1 . fmap sign . unMyApplicative

deriving via (FromPrelude1 (VS.Vector n) a) instance (KnownNat n, Absolute a) => Absolute ((VS.Vector n) a)
