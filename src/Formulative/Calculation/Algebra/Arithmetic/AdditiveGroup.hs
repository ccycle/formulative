module Formulative.Calculation.Algebra.Arithmetic.AdditiveGroup where

import Control.Algebra
import Control.Applicative
import Data.Complex
import qualified Data.Vector.Sized as VS
import Debug.SimpleReflect.Expr
import Formulative.Calculation.Algebra.Arithmetic.Additive
import Formulative.Calculation.Internal.Types
import GHC.Generics
import GHC.Natural
import GHC.TypeNats

class Additive a => AdditiveGroup a where
    (.-.) :: a -> a -> a
    negation :: a -> a
    (.-.) a b = a .+. negation b

    default negation :: (Generic a, GAdditiveGroup (Rep a)) => a -> a
    negation = to . gNegation . from

    infixl 6 .-.

-- | Generic class for Additive Group
class (GAdditive f) => GAdditiveGroup f where
    (..-..) :: f a -> f a -> f a
    gNegation :: f a -> f a
    a ..-.. b = a ..+.. gNegation b

instance GAdditiveGroup a => GAdditiveGroup (M1 i c a) where
    M1 a ..-.. M1 b = M1 (a ..-.. b)
    gNegation (M1 a) = M1 (gNegation a)
instance (GAdditiveGroup a, GAdditiveGroup b) => GAdditiveGroup (a :*: b) where
    (al :*: bl) ..-.. (ar :*: br) = (..-..) al ar :*: (..-..) bl br
    gNegation (al :*: bl) = gNegation al :*: gNegation bl
instance (AdditiveGroup a) => GAdditiveGroup (K1 i a) where
    K1 a ..-.. K1 b = K1 (a .-. b)
    gNegation (K1 a) = K1 (negation a)

----------------------------------------------------------------
-- deriving instance
----------------------------------------------------------------

instance (Num a) => AdditiveGroup (MyNumeric a) where
    negation (MyNumeric a) = MyNumeric (negate a)
    (MyNumeric a) .-. (MyNumeric b) = MyNumeric (a - b)

deriving via (MyNumeric Int) instance AdditiveGroup Int
deriving via (MyNumeric Integer) instance AdditiveGroup Integer
deriving via (MyNumeric Natural) instance AdditiveGroup Natural
deriving via (MyNumeric Word) instance AdditiveGroup Word
deriving via (MyNumeric Float) instance AdditiveGroup Float
deriving via (MyNumeric Double) instance AdditiveGroup Double
deriving via (MyNumeric (Complex Float)) instance AdditiveGroup (Complex Float)
deriving via (MyNumeric (Complex Double)) instance AdditiveGroup (Complex Double)
deriving via (MyNumeric Expr) instance AdditiveGroup Expr

instance {-# OVERLAPS #-} (AdditiveGroup a, AdditiveGroup b) => AdditiveGroup (a -> b) where
    (f .-. g) x = f x .-. g x
    negation f = f . negation
instance AdditiveGroup Bool where
    negation = not

-- a .-. b = a .+. not b

-- applicative
instance (Additive a, AdditiveGroup a, Applicative m) => AdditiveGroup (MyApplicative m a) where
    negation (MyApplicative a) = MyApplicative (fmap negation a)

deriving via (MyApplicative (VS.Vector n) a) instance (KnownNat n, Additive a, AdditiveGroup a) => AdditiveGroup (VS.Vector n a)
deriving via (MyApplicative Maybe a) instance (AdditiveGroup a) => AdditiveGroup (Maybe a)
deriving via (MyApplicative IO a) instance (AdditiveGroup a) => AdditiveGroup (IO a)
deriving via (MyApplicative (Either a) b) instance (AdditiveGroup b) => AdditiveGroup (Either a b)

deriving via (MyApplicative (m :: * -> *) a) instance (AdditiveGroup a, Applicative m, Foldable m) => AdditiveGroup (MyFoldable m a)

integralToSign i x
    | even i = x
    | otherwise = negation x