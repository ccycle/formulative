{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.MonoidInstance where

import GHC.Generics
import LambDEC.Calculation.Operator.Arithmetic.Class

-- deriving (GenericMonoid)とするだけで実装を導出できるようにしたい
-- class GAdditive a => GAdditiveGroup a where
