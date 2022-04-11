{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}

module Formulative.Calculation.Internal.TypeLevelList where

import Data.Constraint

-- import Data.Singletons.Prelude.Instances
import Data.Constraint.Nat
import Data.Proxy
import Data.Reflection
import Data.Singletons

import Data.Singletons.Prelude.List (SList)
import Data.Type.Equality
import Formulative.Calculation.Internal.TypeLevelNatural
import GHC.Natural
import GHC.TypeNats
import Unsafe.Coerce

-- get element by index for NatList
-- >>> import Data.Proxy
-- >>> import GHC.TypeNats
-- >>> natVal (Proxy :: Proxy ('[2,3,5] !! 0))
-- 2
-- >>> natVal (Proxy :: Proxy ('[2,3,5] !! 2))
-- 5
-- >>> natVal (Proxy :: Proxy ('[2,3,5] !! 4))
-- 0
type family (!!) (l :: [Nat]) (n :: Nat) where
  '[] !! _ = 1
  (x ': _) !! 0 = x
  (_ ': xs) !! n = xs !! (n - 1)

(!!!) :: [Natural] -> Natural -> Natural
[] !!! _ = 1
(x : xs) !!! n = if n == 0 then x else xs !!! (n - 1)

-- elemAtKnownNatProof :: forall (ns :: [Nat]) (n :: Nat). (SingI ns, KnownNat n) => SList ns -> SNat n -> SNat (ns !! n)
-- elemAtKnownNatProof proxyns proxyn =
--   let ns = fromSing proxyns
--       n = fromSing proxyn
--       val = ns !!! n
--    in case someNatVal val of
--         SomeNat (_ :: Proxy k) -> unsafeCoerce (SNat :: SNat k)

-- natDict :: KnownNat n => Proxy n -> Dict (KnownNat n)
-- natDict _ = Dict

elemAtKnownNatProof :: (KnownNat n, SingI l) => SList l -> Proxy n -> Dict (KnownNat (l !! n))
elemAtKnownNatProof proxyns proxyn =
  let ns = fromSing proxyns
      n = natVal proxyn
      val = ns !!! n
   in case someNatVal val of
        SomeNat p3 -> unsafeCoerce (natDict p3)

-- -- TODO: モジュールの名前をTypeLevelListから別名にする
-- -- NOTE: 型レベルリスト以外の演算も定義しているのでもう少し正確な名前にしたい
-- leqNat :: forall n m. (KnownNat n, KnownNat m) => Proxy n -> Proxy m -> Maybe ((n <=? m) :~: 'True)
-- leqNat proxyn proxym
--   | natVal proxyn <= natVal proxym = Just (unsafeCoerce Refl)
--   | otherwise = Nothing

-- geqNat :: forall n m. (KnownNat n, KnownNat m) => Proxy n -> Proxy m -> Maybe ((m <=? n) :~: 'True)
-- geqNat = flip leqNat

-- withSomeSingI :: (SingKind k) => Demote k -> (forall (a :: k). (SingI a) => Sing a -> r) -> r
-- withSomeSingI dk f = case toSing dk of
--   SomeSing (s :: Sing a) -> withSingI s (f @a s)

-- withSomeSingIAmbiguous :: (SingKind k) => Demote k -> (forall (a :: k). (SingI a) => r) -> r
-- withSomeSingIAmbiguous dk f = case toSing dk of
--   SomeSing (s :: Sing a) -> withSingI s (f @a)

-- withSomeNat :: Natural -> (forall (a :: Nat). (KnownNat a) => Proxy a -> r) -> r
-- withSomeNat dk f = case someNatVal dk of
--   SomeNat (s :: Proxy a) -> withSingI s (f @a s)