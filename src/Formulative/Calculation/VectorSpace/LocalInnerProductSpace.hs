{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.VectorSpace.LocalInnerProductSpace (
    LocalInnerSpace (..),
) where

import Formulative.Calculation.Internal.Types
import Formulative.Calculation.VectorSpace.VectorSpace
import GHC.Generics
import GHC.Natural

class (VectorSpace v) => LocalInnerSpace v where
    (<|.|>) :: v -> v -> v
    default (<|.|>) :: (Generic v, GLocalInnerSpace (Rep v)) => v -> v -> v
    (<|.|>) a b = to $ (<<..>>) (from a) (from b)

    infixr 7 <|.|>

class GLocalInnerSpace f where
    (<<..>>) :: f v -> f v -> f v
instance LocalInnerSpace s => GLocalInnerSpace (K1 i s) where
    (<<..>>) (K1 v) (K1 w) = K1 $ (<|.|>) v w
instance (GLocalInnerSpace a) => GLocalInnerSpace (M1 i c a) where
    (<<..>>) (M1 v) (M1 w) = M1 $ (<<..>>) v w
instance (GLocalInnerSpace f, GLocalInnerSpace g) => GLocalInnerSpace (f :*: g) where
    (<<..>>) (x :*: y) (z :*: w) = (<<..>>) x z :*: (<<..>>) y w

instance (Num a) => LocalInnerSpace (MyNum a) where
    (MkMyNum a) <|.|> (MkMyNum b) = MkMyNum $ a * b

deriving via (MyNum Int) instance LocalInnerSpace Int
deriving via (MyNum Integer) instance LocalInnerSpace Integer
deriving via (MyNum Natural) instance LocalInnerSpace Natural
deriving via (MyNum Double) instance LocalInnerSpace Double
deriving via (MyNum Float) instance LocalInnerSpace Float