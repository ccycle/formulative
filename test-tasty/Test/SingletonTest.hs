{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Test.SingletonTest where

import qualified Data.Matrix.Static.Dense as D
import qualified Data.Matrix.Static.Generic as G
import Data.Matrix.Static.LinearAlgebra
import qualified Data.Matrix.Static.Sparse as S
import Data.Proxy
import qualified Data.Singletons as S
import qualified Data.Singletons.Prelude as S
import Data.Singletons.Prelude.List hiding (type (!!))
import Data.Singletons.TypeLits
import Data.Type.Equality
import GHC.TypeNats
import HStructure.Calculation.Algebra.Arithmetic.Class
import HStructure.Calculation.Internal.TypeLevelList
import HStructure.Calculation.Matrix.Class
import Test.Tasty
import Unsafe.Coerce (unsafeCoerce)

-- import Data.Singletons.Prelude.Num

-- Proxy a -> S.Sing a

printNats :: forall (ns :: [Nat]). S.Sing ns -> IO ()
printNats ss = case ss of
    SNil ->
        return ()
    s `SCons` ss' -> do
        print $ S.fromSing s
        printNats ss'

-- typeFamilyKnownNatProofUnsafe :: forall (ns :: [Nat]) (n :: Nat). (S.SingI ns, KnownNat n) => SList ns -> SNat n -> SNat (ns !! n)
-- typeFamilyKnownNatProofUnsafe proxyns proxyn = unsafeCoerce (SNat)

-- typeFamilyKnownNatTest :: forall (ns :: [Nat]) (n :: Nat). (S.SingI ns, KnownNat n) => S.Sing ns -> S.Sing n -> IO ()
-- typeFamilyKnownNatTest _ _ = case (SNat :: SNat (ns !! n)) of
--     (SNat) -> printFromSing (S.Sing :: S.Sing (ns !! n))

-- Nothing -> error ""

-- >>> import GHC.Natural
-- >>> a1 = 2 :: Natural
-- >>> a2 = 5 :: Natural
-- >>> b = map fromIntegral [a1,a2] :: [Demote Nat]
-- >>> S.withSomeSing b printNats

nInput = 2
nsInput = [2, 3, 5]

printFromSing :: forall (n :: Nat). S.Sing n -> IO ()
printFromSing proxy = print $ S.fromSing proxy

printFromProxy :: forall n. (KnownNat n) => Proxy n -> IO ()
printFromProxy proxy = print $ natVal proxy

-- isGreaterThan1 proxy = testEquality ((SNat :: S.Sing 1) %<= proxy) STrue

-- constrainedFunction :: forall n. (SingI (n - 1), (1 <= n) ~ 'True) => Matrix (n - 1) (n - 2) Double
-- constrainedFunction :: forall n. ((1 S.<= n) ~ 'True, S.SingI (n S.- 1)) => Matrix (n S.- 1) (n S.- 1) Double
-- constrainedFunction = zero

-- runtimeExecTest :: Integral a => a -> IO ()
-- runtimeExecTest n = S.withSomeSing (fromIntegral n) $ \sn@(SNat :: S.Sing n) ->
--     let s1 = SNat :: SNat 1; s2 = SNat :: SNat 2
--      in case testEquality (s1 S.%<= sn) S.STrue of
--             Just Refl -> print (constrainedFunction :: Matrix (n S.- 1) (n S.- 1) Double)
--             Nothing -> error "test"

-- >>> S.withSomeSing nInput printFromSing
-- "2"

unit_printNatsTest = printNats (S.Sing :: S.Sing [1, 2, 3, 4])
unit_sing1 = S.withSomeSing nInput printFromSing
unit_sing2 = S.withSomeSing nsInput printNats