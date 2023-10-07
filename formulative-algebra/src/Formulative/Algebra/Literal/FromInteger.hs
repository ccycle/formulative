module Formulative.Algebra.Literal.FromInteger where

import qualified Data.Vector.Sized as VS
import Debug.SimpleReflect.Expr
import Formulative.Algebra.Arithmetic.Multiplicative
import Formulative.Algebra.Internal.FromPrelude
import GHC.Generics
import GHC.Natural
import GHC.TypeNats
import Prelude hiding (fromInteger, (*))
import qualified Prelude

class FromInteger a where
    fromInteger :: Integer -> a
    default fromInteger :: (Generic a, GFromInteger (Rep a)) => Integer -> a
    fromInteger = to . gfromInteger

instance (Multiplicative a, FromInteger a) => FromInteger (a -> a) where
    fromInteger n = (*) (fromInteger n)

class GFromInteger f where
    gfromInteger :: Integer -> f a
instance GFromInteger a => GFromInteger (M1 i c a) where
    gfromInteger a = M1 (gfromInteger a)
instance (GFromInteger a, GFromInteger b) => GFromInteger (a :*: b) where
    gfromInteger a = gfromInteger a :*: gfromInteger a
instance (FromInteger a) => GFromInteger (K1 i a) where
    gfromInteger a = K1 (fromInteger a)

instance (Num a) => FromInteger (FromPrelude a) where
    fromInteger = FromPrelude . Prelude.fromInteger

instance (FromInteger a, Applicative m) => FromInteger (FromPrelude1 m a) where
    fromInteger = FromPrelude1 . pure . fromInteger

deriving via (FromPrelude Int) instance FromInteger Int
deriving via (FromPrelude Integer) instance FromInteger Integer
deriving via (FromPrelude Word) instance FromInteger Word
deriving via (FromPrelude Natural) instance FromInteger Natural
deriving via (FromPrelude Rational) instance FromInteger Rational
deriving via (FromPrelude Float) instance FromInteger Float
deriving via (FromPrelude Double) instance FromInteger Double
deriving via (FromPrelude Expr) instance FromInteger Expr

deriving via (FromPrelude1 (VS.Vector n) a) instance (KnownNat n, FromInteger a) => FromInteger (VS.Vector n a)