module Formulative.Calculation.Internal.IfThenElse where

class IfThenElse b where
    ifThenElse :: b -> a -> a -> a

instance IfThenElse Bool where
    ifThenElse True t _ = t
    ifThenElse False _ f = f