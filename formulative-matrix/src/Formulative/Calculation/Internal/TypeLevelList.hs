{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}

module Formulative.Calculation.Internal.TypeLevelList where

import Data.Constraint
import Data.Proxy
import Data.Singletons
import Data.Singletons.Prelude.List (SList)
import Formulative.Calculation.Internal.TypeLevelNatural
import GHC.Natural
import GHC.TypeNats
import Unsafe.Coerce

{- | Get element by index for list of type-level Natural.
 >>> import Data.Proxy
 >>> import GHC.TypeNats
 >>> natVal (Proxy :: Proxy ('[2,3,5] !! 0))
 2
 >>> natVal (Proxy :: Proxy ('[2,3,5] !! 2))
 5
 >>> natVal (Proxy :: Proxy ('[2,3,5] !! 4))
 0
-}
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

elemAtKnownNatProof :: (KnownNat n, SingI l) => SList l -> Proxy n -> Dict (KnownNat (l !! n))
elemAtKnownNatProof proxyns proxyn =
  let ns = fromSing proxyns
      n = natVal proxyn
      val = ns !!! n
   in case someNatVal val of
        SomeNat p3 -> unsafeCoerce (natDict p3)
