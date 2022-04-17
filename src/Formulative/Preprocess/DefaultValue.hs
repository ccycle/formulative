module Formulative.Preprocess.DefaultValue where

import Formulative.Calculation.Internal.Types
import GHC.Generics
import GHC.Natural

class HasDefaultValue a where
    defaultValue :: a
    default defaultValue :: (Generic a, GDefaultValue (Rep a)) => a
    defaultValue = to gdefaultValue
class GDefaultValue f where
    gdefaultValue :: f a
instance (GDefaultValue a, GDefaultValue b) => GDefaultValue (a :*: b) where
    gdefaultValue = gdefaultValue :*: gdefaultValue
instance (HasDefaultValue a) => GDefaultValue (K1 i a) where
    gdefaultValue = K1 defaultValue
instance (GDefaultValue a) => GDefaultValue (M1 i c a) where
    gdefaultValue = M1 gdefaultValue

instance (Num a) => HasDefaultValue (MyNum a) where
    defaultValue = MyNum 0

deriving via (MyNum Natural) instance HasDefaultValue Natural
deriving via (MyNum Int) instance HasDefaultValue Int
deriving via (MyNum Integer) instance HasDefaultValue Integer
deriving via (MyNum Float) instance HasDefaultValue Float
deriving via (MyNum Double) instance HasDefaultValue Double
