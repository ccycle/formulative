{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

module Test.SingletonTest where

import qualified Data.Matrix.Static.Dense as D
import qualified Data.Matrix.Static.Generic as G

-- import Data.Matrix.Static.LinearAlgebra
import qualified Data.Matrix.Static.Sparse as S
import Data.Proxy
import Data.Singletons (Sing, SingI (sing), SingInstance (SingInstance), SingKind (toSing), SomeSing (SomeSing), singInstance)
import qualified Data.Singletons as S
import Data.Singletons.Prelude (SingKind (fromSing))
import qualified Data.Singletons.Prelude as S
import Data.Singletons.Prelude.List hiding (type (!!))
import Data.Singletons.Prelude.Tuple (STuple2)
import Data.Singletons.TH
import Data.Singletons.TypeLits

-- import Data.Type.Equality
import GHC.Natural
import GHC.TypeNats

-- import Formulative.Calculation.Algebra.Arithmetic

-- import Formulative.Calculation.DiscreteExteriorCalculus.Class
-- import Formulative.Calculation.DiscreteExteriorCalculus.Proofs

import Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Types
import Formulative.Calculation.Internal.Singletons
import Formulative.Calculation.Internal.TypeLevelList
import Formulative.Calculation.Internal.TypeLevelNatural
import Formulative.Calculation.Matrix.Class
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

printTypeParameterTest1 :: forall n c. (SingI n, SingI c) => SNat n -> SCellType c -> IO ()
printTypeParameterTest1 n c = putStrLn $ show (fromSing n) <> show (fromSing c)

printTypeParameterTest2 = withSomeSingI Primal $ withSomeSingI 1 printTypeParameterTest1

printTypeParameterTest3 :: forall n c. (KnownNat n, SingI c) => Proxy n -> SCellType c -> IO ()
printTypeParameterTest3 n c = putStrLn $ show (natVal n) <> show (fromSing c)
printTypeParameterTest4 = withSomeSingI Primal $ withSomeNat 1 printTypeParameterTest3

printTypeParameterTest5 :: forall n (c :: CellType). (KnownNat n, SingI c) => IO ()
printTypeParameterTest5 = putStrLn $ show (natVal (Proxy :: Proxy n)) <> show (fromSing (sing :: SCellType c))
printTypeParameterTest6 = case (someNatVal (1 :: Natural), toSing Primal) of (SomeNat (p :: Proxy n), SomeSing (c :: SCellType a)) -> case singInstance c of SingInstance -> printTypeParameterTest5 @n @a

-- printTypeParameterTest7 :: forall (c :: CellType). (SingI c) => IO ()
-- printTypeParameterTest7 = print (fromSing (sing :: SCellType c))
-- printTypeParameterTest8 = withSomeSingIAmbiguous2 Primal printTypeParameterTest7

$( singletons
    [d|
        data RGB = R | G | B
            deriving (Show, Eq)

        rotate :: RGB -> RGB
        rotate R = G
        rotate G = B
        rotate B = R
        |]
 )

unit_printNatsTest = printNats (S.Sing :: S.Sing [1, 2, 3, 4])
unit_sing1 = S.withSomeSing nInput printFromSing
unit_sing2 = S.withSomeSing nsInput printNats