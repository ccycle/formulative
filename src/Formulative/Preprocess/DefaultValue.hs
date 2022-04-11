module Formulative.Preprocess.DefaultValue where

import GHC.Generics

-- TODO: Create a DefaultValue class to hash the user-defined file
class DefaultValue a where
    defaultValue :: a
    default defaultValue :: (Generic a, GDefaultValue (Rep a)) => a
    defaultValue = to gdefaultValue
class GDefaultValue f where
    gdefaultValue :: f a
instance (GDefaultValue a, GDefaultValue b) => GDefaultValue (a :*: b) where
    gdefaultValue = gdefaultValue :*: gdefaultValue
instance (DefaultValue a) => GDefaultValue (K1 i a) where
    gdefaultValue = K1 defaultValue
instance (GDefaultValue a) => GDefaultValue (M1 i c a) where
    gdefaultValue = M1 gdefaultValue