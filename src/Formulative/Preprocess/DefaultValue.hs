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

instance (Num a) => HasDefaultValue (MyNumeric a) where
    defaultValue = MyNumeric 0

deriving via (MyNumeric Natural) instance HasDefaultValue Natural
deriving via (MyNumeric Int) instance HasDefaultValue Int
deriving via (MyNumeric Integer) instance HasDefaultValue Integer
deriving via (MyNumeric Float) instance HasDefaultValue Float
deriving via (MyNumeric Double) instance HasDefaultValue Double
