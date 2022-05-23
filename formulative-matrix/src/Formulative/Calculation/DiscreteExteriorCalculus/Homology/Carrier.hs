{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.DiscreteExteriorCalculus.Homology.Carrier where

import Control.Algebra
import Control.Carrier.Reader
import Formulative.Calculation.DiscreteExteriorCalculus.Homology.Effect
import Formulative.Calculation.DiscreteExteriorCalculus.Homology.Types

newtype ConnectivityC n l m a = ConnectivityC {runConnectivityC :: ReaderC (Simplices n l) m a}
    deriving stock (Functor)
    deriving newtype (Applicative, Monad)
instance (Algebra sig m) => Algebra (Connectivity n l :+: sig) (ConnectivityC n l m) where
    alg hdl sig ctx = case sig of
        L GetPrimitiveSimplicies -> do
            l <- ConnectivityC (ask @(Simplices n l))
            pure ((<$ ctx) l)
        R other -> ConnectivityC (alg (runConnectivityC . hdl) (R other) ctx)

runConnectivity :: Simplices n l -> ConnectivityC n l m a -> m a
runConnectivity s = runReader s . runConnectivityC