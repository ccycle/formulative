{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

-- {-# LANGUAGE UndecidableInstances #-}

module Test.GenericsTest where

import GHC.Generics

import HStructure.Calculation.Algebra.Arithmetic.Class
import HStructure.Calculation.VectorSpace.Class

-- import HStructure.Calculation.Algebra.Arithmetic.Class( Scalar(..) )

import GHC.Natural

-- https://hackage.haskell.org/package/generic-monoid-0.1.0.1/src/src/Data/Monoid/Generic.hs

-- ---------------------------------------------------------------------------

-- -- | Additive

-- ---------------------------------------------------------------------------
-- class Additive a where
--     (.+.) :: a -> a -> a
--     zero :: a

--     default (.+.) :: (Generic a, GAdditive (Rep a)) => a -> a -> a
--     (.+.) a b = to $ (..+..) (from a) (from b)
--     default zero :: (Generic a, GAdditive (Rep a)) => a
--     zero = to gZero

--     infixr 6 .+.

-- class GAdditive f where
--     (..+..) :: f a -> f a -> f a
--     gZero :: f a
-- instance GAdditive a => GAdditive (M1 i c a) where
--     M1 a ..+.. M1 b = M1 (a ..+.. b)
--     gZero = M1 gZero
-- instance (GAdditive a, GAdditive b) => GAdditive (a :*: b) where
--     (al :*: bl) ..+.. (ar :*: br) = (..+..) al ar :*: (..+..) bl br
--     gZero = gZero :*: gZero
-- instance Additive a => GAdditive (K1 i a) where
--     K1 a ..+.. K1 b = K1 (a .+. b)
--     gZero = K1 zero

-- ---------------------------------------------------------------------------

-- -- | Additive Group

-- ---------------------------------------------------------------------------
-- class Additive a => AdditiveGroup a where
--     (.-.) :: a -> a -> a
--     negation :: a -> a

--     default (.-.) :: (Generic a, GAdditiveGroup (Rep a)) => a -> a -> a
--     (.-.) a b = to $ from a ..-.. from b
--     default negation :: (Generic a, GAdditiveGroup (Rep a)) => a -> a
--     negation = to . gNegation . from

--     infixr 6 .-.

-- -- | Generic class for Additive Group
-- class GAdditiveGroup f where
--     (..-..) :: f a -> f a -> f a
--     gNegation :: f a -> f a

-- instance GAdditiveGroup a => GAdditiveGroup (M1 i c a) where
--     M1 a ..-.. M1 b = M1 (a ..-.. b)
--     gNegation (M1 a) = M1 (gNegation a)
-- instance (GAdditiveGroup a, GAdditiveGroup b) => GAdditiveGroup (a :*: b) where
--     (al :*: bl) ..-.. (ar :*: br) = (..-..) al ar :*: (..-..) bl br
--     gNegation (al :*: bl) = gNegation al :*: gNegation bl
-- instance (AdditiveGroup a) => GAdditiveGroup (K1 i a) where
--     K1 a ..-.. K1 b = K1 (a .-. b)
--     gNegation (K1 a) = K1 (negation a)

-- ---------------------------------------------------------------------------

-- -- | Multiplicative

-- ---------------------------------------------------------------------------
-- class Multiplicative a where
--     (.*.) :: a -> a -> a
--     one :: a

--     default (.*.) :: (Generic a, GMultiplicative (Rep a)) => a -> a -> a
--     (.*.) a b = to $ (..*..) (from a) (from b)
--     default one :: (Generic a, GMultiplicative (Rep a)) => a
--     one = to gOne

--     infixr 7 .*.

-- class GMultiplicative f where
--     (..*..) :: f a -> f a -> f a
--     gOne :: f a
-- instance GMultiplicative a => GMultiplicative (M1 i c a) where
--     M1 a ..*.. M1 b = M1 (a ..*.. b)
--     gOne = M1 gOne
-- instance (GMultiplicative a, GMultiplicative b) => GMultiplicative (a :*: b) where
--     (al :*: bl) ..*.. (ar :*: br) = (..*..) al ar :*: (..*..) bl br
--     gOne = gOne :*: gOne
-- instance Multiplicative a => GMultiplicative (K1 i a) where
--     K1 a ..*.. K1 b = K1 (a .*. b)
--     gOne = K1 one

-- -------------------------------------------------------------------------------

-- -- | Vector space

-- -------------------------------------------------------------------------------
-- class (AdditiveGroup a) => VectorSpace a where
--     type Scalar a :: *
--     (*.) :: Scalar a -> a -> a
--     default (*.) :: (Generic a, GVectorSpace (Rep a), GAdditive (Rep a), GAdditiveGroup (Rep a), Scalar a ~ GScalar (Rep a)) => Scalar a -> a -> a
--     type Scalar a = GScalar (Rep a)
--     (*.) a b = to (a *.. from b)

-- -- instance (Generic a, GVectorSpace (Rep a), GAdditive (Rep a), GAdditiveGroup (Rep a)) => VectorSpace a
-- -- type Scalar a = GScalar (Rep a)
-- -- (*.) a b = to (a *.. from b)

-- class GVectorSpace f where
--     type GScalar f
--     (*..) :: GScalar f -> f a -> f a

-- instance GVectorSpace f => GVectorSpace (M1 i c f) where
--     type GScalar (M1 i c f) = GScalar f
--     a *.. M1 b = M1 (a *.. b)

-- instance VectorSpace a => GVectorSpace (K1 i a) where
--     type GScalar (K1 i a) = Scalar a
--     a *.. K1 b = K1 (a *. b)

-- instance (GVectorSpace f, GVectorSpace g, GScalar f ~ GScalar g) => GVectorSpace (f :*: g) where
--     type GScalar (f :*: g) = GScalar f
--     μ *.. (x :*: y) = μ *.. x :*: μ *.. y

-- -------------------------------------------------------------------------------

-- -- | Norm Space

-- -------------------------------------------------------------------------------
-- class NormSpace v where
--     type RealField v
--     norm :: v -> RealField v
--     default norm :: (Generic v, GInnerSpace (Rep v), GScalar (Rep v) ~ RealField v) => v -> RealField v
--     type RealField v = GScalar (Rep v)
--     norm = gNorm . from

-- class GInnerSpace f where
--     type GScalar f
--     gNorm :: f v -> GScalar f

-- instance NormSpace s => GInnerSpace (K1 i s) where
--     type GScalar (K1 i s) = (RealField s)
--     gNorm (K1 w) = norm w

-- instance (GInnerSpace a) => GInnerSpace (M1 i c a) where
--     gNorm (M1 w) = gNorm w
--     type GScalar (M1 i c a) = GScalar a

-- instance (GInnerSpace f, GInnerSpace g, GScalar g ~ GScalar f, Additive (GScalar f)) => GInnerSpace (f :*: g) where
--     type GScalar (f :*: g) = GScalar f
--     gNorm (x :*: y) = gNorm x .+. gNorm y

-- --------------------------------------------------------
-- -- Double
-- --------------------------------------------------------
-- instance Additive Double where
--     (.+.) = (+)
--     zero = 0
-- instance AdditiveGroup Double where
--     (.-.) = (-)
--     negation = negate

-- instance Multiplicative Double where
--     (.*.) = (*)
--     one = 1

-- instance VectorSpace Double where
--     type Scalar Double = Double
--     (*.) = (*)

-- instance NormSpace Double where
--     type RealField Double = Double
--     norm = abs

-- --------------------------------------------------------
-- -- Double
-- --------------------------------------------------------
-- instance Additive Float where
--     (.+.) = (+)
--     zero = 0
-- instance AdditiveGroup Float where
--     (.-.) = (-)
--     negation = negate

-- instance Multiplicative Float where
--     (.*.) = (*)
--     one = 1

-- instance VectorSpace Float where
--     type Scalar Float = Float
--     (*.) = (*)

-- instance NormSpace Float where
--     type RealField Float = Float
--     norm = abs

-- --------------------------------------------------------
-- -- Int
-- --------------------------------------------------------

-- instance Additive Int where
--     (.+.) = (+)
--     zero = 0

-- instance AdditiveGroup Int where
--     (.-.) = (-)
--     negation = negate

-- instance Multiplicative Int where
--     (.*.) = (*)
--     one = 1

-- instance VectorSpace Int where
--     type Scalar Int = Int
--     (*.) = (*)

-- --------------------------------------------------------
-- -- Natural
-- --------------------------------------------------------

-- instance Additive Natural where
--     (.+.) = (+)
--     zero = 0

-- -- instance AdditiveGroup Natural where
-- --     (.-.) = (-)
-- --     negation = negate

-- instance Multiplicative Natural where
--     (.*.) = (*)
--     one = 1

-- -- instance VectorSpace Natural where
-- --     type Scalar Natural = Natural
-- --     (*.) = (*)

-- --------------------------------------------------------
-- -- Integer
-- --------------------------------------------------------

-- instance Additive Integer where
--     (.+.) = (+)
--     zero = 0

-- instance AdditiveGroup Integer where
--     (.-.) = (-)
--     negation = negate

-- instance Multiplicative Integer where
--     (.*.) = (*)
--     one = 1

-- instance VectorSpace Integer where
--     type Scalar Integer = Integer
--     (*.) = (*)

-------------------------------------------------------------------------------

-- | Inner Space

-------------------------------------------------------------------------------
-- class (VectorSpace v) => InnerSpace v where
--     (<.>) :: v -> v -> Scalar v
--     default (<.>) :: (Generic v, GInnerSpace (Rep v), GScalar (Rep v) ~ Scalar v) => v -> v -> Scalar v
--     (<.>) a b = (<..>) (from a) (from b)

-- class GInnerSpace f where
--     (<..>) :: f v -> f v -> GScalar f

-- instance InnerSpace s => GInnerSpace (K1 i s) where
--     (<..>) (K1 v) (K1 w) = (<.>) v w

-- instance (GInnerSpace a) => GInnerSpace (M1 i c a) where
--     (<..>) (M1 v) (M1 w) = (<..>) v w

-- instance (GInnerSpace f, GInnerSpace g, GScalar g ~ GScalar f, Additive (GScalar f)) => GInnerSpace (f :*: g) where
--     (<..>) (x :*: y) (z :*: w) = (<..>) x z .+. (<..>) y w

--------------------------------------------------------
-- Test
--------------------------------------------------------

data Rec = Rec {pressure :: Double, density :: Double}
    deriving stock (Show, Eq, Generic)
    deriving anyclass (VectorSpace, Additive, AdditiveGroup, NormSpace, Multiplicative)

unit_RecAdd = print $ Rec 1 1 .+. Rec 2 3
unit_RecSub = print $ Rec 1 1 .-. Rec 2 3
unit_RecMul = print $ Rec 2 2 .*. Rec 2 3
unit_RecZero = print (zero :: Rec)

unit_testRecNorm = print $ norm (Rec 1 2)

unit_RecVectorSpace = print $ 2 *. Rec 2 3

data HeteroRecTest = HeteroRecTest {pressure :: Double, density :: Int}
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Additive, AdditiveGroup, Multiplicative)

unit_HeteroRecAdd = print $ HeteroRecTest 1 1 .+. HeteroRecTest 1 2
unit_HeteroRecSub = print $ HeteroRecTest 1 1 .-. HeteroRecTest 1 2

associativity :: (Eq a) => (a -> a -> a) -> a -> a -> a -> Bool
associativity (<>) a b c = (a <> b) <> c == a <> (b <> c)

prop_associativity_DoubleAdd_fail = associativity @Double (.+.) -- fail
prop_associativity_DoubleMul_fail = associativity @Double (.*.) -- fail
prop_associativity_FloatAdd = associativity @Float (.+.) -- fail
prop_associativity_FloatMul = associativity @Float (.*.) -- fail

prop_associativity_IntAdd = associativity @Int (.+.)
prop_associativity_IntMul = associativity @Int (.*.)

prop_associativity_IntegerAdd = associativity @Integer (.+.)
prop_associativity_IntegerMul = associativity @Integer (.*.)

data HeteroRecFieldTest = HeteroRecFieldTest {pressure :: Double, density :: Float}
    deriving stock (Show, Eq, Generic)
    deriving anyclass (Additive, AdditiveGroup, Multiplicative)

-- Fieldにするのは相当しんどいしあまり使い道がないので一旦は置いとく