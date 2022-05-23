{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}

module Formulative.Calculation.Internal.TypeLevelNatural where

import Control.Exception.Safe
import Data.Constraint (Dict (..))
import Data.Proxy
import Data.Type.Equality
import GHC.Natural
import GHC.TypeNats
import Unsafe.Coerce (unsafeCoerce)

natDict :: KnownNat n => Proxy n -> Dict (KnownNat n)
natDict _ = Dict

data CompareNatException
    = LEQNatError Natural Natural
    | GEQNatError Natural Natural
    | EQNatError Natural Natural
    deriving (Show)
instance Exception CompareNatException where
    displayException x = case x of
        (LEQNatError a b) -> f "≤" a b
        (GEQNatError a b) -> f "≥" a b
        (EQNatError a b) -> f "=" a b
      where
        f o a b =
            concat
                [ "*** CompareNatException: The values does not meet requirement "
                , "("
                , show a
                , ")"
                , o
                , "("
                , show b
                , ")"
                ]

leqNat :: forall m n1 n2. (KnownNat n1, KnownNat n2, MonadThrow m) => Proxy n1 -> Proxy n2 -> m ((n1 <=? n2) :~: 'True)
leqNat proxyn1 proxyn2
    | natVal proxyn1 <= natVal proxyn2 = return (unsafeCoerce Refl)
    | otherwise = throw (LEQNatError n1val n2val)
  where
    n1val = natVal proxyn1
    n2val = natVal proxyn2

geqNat :: forall m n1 n2. (KnownNat n1, KnownNat n2, MonadThrow m) => Proxy n1 -> Proxy n2 -> m ((n2 <=? n1) :~: 'True)
geqNat = flip leqNat

-----------
-- Data.Reflection
newtype MagicNat r = MagicNat (forall (n :: Nat). KnownNat n => Proxy n -> r)

withSomeNat :: forall r. Natural -> (forall (n :: Nat). KnownNat n => Proxy n -> r) -> r
withSomeNat n k =
    unsafeCoerce
        (MagicNat k :: MagicNat r)
        (n :: Natural)
        Proxy

-----------