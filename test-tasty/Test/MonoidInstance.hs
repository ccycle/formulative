{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE UndecidableInstances #-}

module Test.MonoidInstance where

import GHC.Generics
import HStructure.Calculation.Algebra.Arithmetic.Class

-- deriving (GenericMonoid)とするだけで実装を導出できるようにしたい
-- class GAdditive a => GAdditiveGroup a where
