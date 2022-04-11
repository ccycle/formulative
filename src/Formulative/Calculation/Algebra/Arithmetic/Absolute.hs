module Formulative.Calculation.Algebra.Arithmetic.Absolute where

import qualified Data.Vector.Sized as VS
import Formulative.Calculation.Algebra.Arithmetic.Ring
import Formulative.Calculation.Algebra.Arithmetic.Rng
import Formulative.Calculation.Internal.Types
import GHC.TypeNats

class (Ring a) => Absolute a where
    abs' :: a -> a
    signum' :: a -> a

instance (Num a) => Absolute (MyNum a) where
    abs' = MkMyNum . Prelude.abs . unMkMyNum
    signum' = MkMyNum . Prelude.signum . unMkMyNum

deriving via (MyNum Double) instance Absolute Double
deriving via (MyNum Float) instance Absolute Float
deriving via (MyNum Int) instance Absolute Int
deriving via (MyNum Integer) instance Absolute Integer

-- applicative
instance (Absolute a, Applicative m) => Absolute (MyApplicative m a) where
    abs' = MkMyApplicative . fmap abs' . unMkMyApplicative
    signum' = MkMyApplicative . fmap signum' . unMkMyApplicative

deriving via (MyApplicative IO a) instance (Absolute a) => Absolute (IO a)
deriving via (MyApplicative Maybe a) instance (Absolute a) => Absolute (Maybe a)
deriving via (MyApplicative (Either a) b) instance (Absolute b) => Absolute ((Either a) b)
deriving via (MyApplicative (VS.Vector n) a) instance (KnownNat n, Absolute a) => Absolute ((VS.Vector n) a)