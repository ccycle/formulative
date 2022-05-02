{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Test.GenericsTest where

import Control.Algebra
import Control.Applicative
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv as Csv
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Proofs
import Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Types
import Formulative.Calculation.Internal.Types
import Formulative.Calculation.VectorSpace.Class
import GHC.Generics
import GHC.Natural

-- https://hackage.haskell.org/package/generic-monoid-0.1.0.1/src/src/Data/Monoid/Generic.hs

data Rec = Rec {pressure :: Double, density :: Double}
    deriving stock (Generic, Show, Eq)
    deriving anyclass (VectorSpace, Additive, AdditiveGroup, NormSpace, Multiplicative)

unit_RecAdd = print $ Rec 1 1 .+. Rec 2 3
unit_RecSub = print $ Rec 1 1 .-. Rec 2 3
unit_RecMul = print $ Rec 2 2 .*. Rec 2 3
unit_RecZero = print (zero :: Rec)

unit_testRecNorm = print $ norm (Lp 2) (Rec 1 2)

unit_RecVectorSpace = print $ 2 *. Rec 2 3

data HeteroRecTest = HeteroRecTest {pressure :: Double, density :: Int}
    deriving stock (Generic, Show, Eq)
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
    deriving stock (Generic, Show, Eq)
    deriving anyclass (Additive, AdditiveGroup, Multiplicative, Semiring, Ring, Field)

data DiffFormRecFieldTest = DiffFormRecFieldTest {pressure :: DifferentialForm 2 '[2, 5, 4] Dual 0 Double, density :: DifferentialForm 2 '[2, 5, 4] Dual 0 Double, hodge :: HodgeStar 2 '[2, 5, 4] Dual 0 Double}
    deriving stock (Show, Generic)
    deriving anyclass (Additive, AdditiveGroup, Multiplicative, VectorSpace)

toRecordTest = Csv.genericToNamedRecord Csv.defaultOptions (HeteroRecFieldTest zero zero)
writeFileTest = BSL.writeFile "testDouble.csv" $ BSL.fromStrict (fromJust $ HM.lookup "pressure" toRecordTest)

-- toRecordDiffTest = Csv.genericToNamedRecord Csv.defaultOptions (DiffFormRecFieldTest zero zero zero)
-- writeFileTest2 = BSL.writeFile "testSparseMatrix.csv" $ BSL.fromStrict (fromJust $ HM.lookup "pressure" toRecordDiffTest)
-- writeFileTest3 = BSL.writeFile "testHodgeStarMatrix.csv" $ BSL.fromStrict (fromJust $ HM.lookup "hodge" toRecordDiffTest)

unit_lookup = print $ HM.lookup "pressure" toRecordTest