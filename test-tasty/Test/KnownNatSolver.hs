{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module Test.KnownNatSolver where

import Data.Proxy (Proxy (..))
import Data.Type.Bool (If)
import qualified Data.Vector.Sized as VS
import GHC.Natural
import GHC.TypeLits.KnownNat
import GHC.TypeNats
import Test.Tasty

instance (KnownNat a, KnownNat b) => KnownNat2 $(nameToSymbol ''Max) a b where
    natSing2 =
        let x = natVal (Proxy :: Proxy a)
            y = natVal (Proxy :: Proxy b)
            z = max x y
         in SNatKn z
    {-# INLINE natSing2 #-}

type family Max (a :: Nat) (b :: Nat) :: Nat where
    Max 0 b = b
    Max a b = If (a <=? b) b a

knownNatTest :: forall n k. (KnownNat n, KnownNat k) => Proxy n -> Proxy k -> VS.Vector (Max n k) Int
knownNatTest _ _ = VS.replicate 1
