module Formulative.Calculation.Internal.Singletons where

import Data.Singletons

withSomeSingI :: (SingKind k) => Demote k -> (forall (a :: k). (SingI a) => Sing a -> r) -> r
withSomeSingI dk f = case toSing dk of
    SomeSing (s :: Sing a) -> withSingI s (f @a s)

-- GHC.TypeNats.SomeNat
data SomeSingI k = forall (a :: k). SingI a => SomeSingI (Sing a)

-- someNatValに相当する関数
someSingVal :: SingKind k => Demote k -> SomeSingI k
someSingVal x = case toSing x of SomeSing y -> case singInstance y of SingInstance -> SomeSingI y

-- someSingVal x = case toSing x of SomeSing y -> case singInstance y of SingInstance ->
-- withSomeSingIAmbiguous :: (SingKind k) => Demote k -> (forall (a :: k). (SingI a) => r) -> r
-- withSomeSingIAmbiguous dk f = case toSing dk of
--     SomeSing (s :: Sing a) -> withSingI s (f @a)