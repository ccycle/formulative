module Formulative.Preprocess.DefaultValue where

import GHC.Generics

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