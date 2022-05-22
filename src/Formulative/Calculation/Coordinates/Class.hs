module Formulative.Calculation.Coordinates.Class where

class HasCoordinateTransformation f g a where
    transformCoord :: f a -> g a
