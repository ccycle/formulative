{-# LANGUAGE AllowAmbiguousTypes #-}

module Formulative.Calculation.Algebra.SetTheory.FiniteField where

import Data.Foldable
import Data.Proxy
import Formulative.Calculation.Algebra.Arithmetic.Class
import Formulative.Calculation.Internal.Types
import GHC.Generics
import GHC.Natural
import GHC.TypeNats

-- import Data.FiniteField <- Genericsが使えない、値コンストラクタをエクスポートしてないからインスタンスを定義できない
-- 自作する必要あり？

-- import GHC.Natural

-- 要素数が有限の体
class (Field k, KnownNat n) => FiniteField n k where
    -- Sized List
    allElements :: forall n v. Foldable (v n) => v n k
    cardinality :: Natural
    cardinality = natVal (Proxy :: Proxy n)

newtype PrimeField (p :: Nat) = PrimeField Integer deriving (Show, Eq, Generic)

instance (KnownNat p) => Additive (PrimeField p) where
    (.+.) a b = undefined
    zero = undefined

instance (KnownNat p) => AdditiveGroup (PrimeField p) where
    (.-.) a b = undefined
    negation = undefined

instance (KnownNat p) => Multiplicative (PrimeField p) where
    (.*.) a b = undefined
    one = undefined

instance (KnownNat p) => Semiring (PrimeField p) where
    fromInteger = undefined

instance (KnownNat p) => Ring (PrimeField p)

-- p が素数の場合のみ体になる
-- 型レベル制約を書きたい
instance (KnownNat p) => Field (PrimeField p) where
    reciprocal = undefined
    fromRational' = undefined
    isDividable = undefined