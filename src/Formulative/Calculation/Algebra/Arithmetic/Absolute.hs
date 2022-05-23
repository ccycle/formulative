module Formulative.Calculation.Algebra.Arithmetic.Absolute where

import qualified Data.Vector.Sized as VS
import Formulative.Calculation.Algebra.Arithmetic.Ring
import Formulative.Calculation.Algebra.Arithmetic.Semiring
import Formulative.Calculation.Internal.Types
import GHC.TypeNats

class (Ring a) => Absolute a where
    absolute :: a -> a
    sign :: a -> a

instance (Num a) => Absolute (MyNum a) where
    absolute = MyNum . Prelude.abs . unMyNum
    sign = MyNum . Prelude.signum . unMyNum

deriving via (MyNum Double) instance Absolute Double
deriving via (MyNum Float) instance Absolute Float
deriving via (MyNum Int) instance Absolute Int
deriving via (MyNum Integer) instance Absolute Integer

-- applicative
instance (Absolute a, Applicative m) => Absolute (MyApplicative m a) where
    absolute = MyApplicative . fmap absolute . unMyApplicative
    sign = MyApplicative . fmap sign . unMyApplicative

deriving via (MyApplicative IO a) instance (Absolute a) => Absolute (IO a)
deriving via (MyApplicative Maybe a) instance (Absolute a) => Absolute (Maybe a)
deriving via (MyApplicative (Either a) b) instance (Absolute b) => Absolute ((Either a) b)
deriving via (MyApplicative (VS.Vector n) a) instance (KnownNat n, Absolute a) => Absolute ((VS.Vector n) a)
