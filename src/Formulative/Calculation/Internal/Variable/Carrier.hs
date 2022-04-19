{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.Internal.Variable.Carrier where

import Control.Algebra
import Control.Carrier.State.Strict
import Formulative.Calculation.Internal.Class
import Formulative.Calculation.Internal.Variable.Effect

newtype VariableC a m b = VariableC {runVariableC :: StateC (VariableOld a) (StateC (VariableNew a) m) b}
    deriving stock (Functor)
    deriving newtype (Applicative, Monad)
instance (Algebra sig m) => Algebra (Variable a :+: sig) (VariableC a m) where
    alg hdl sig ctx = case sig of
        L GetVariableOld -> do
            env <- VariableC (get @(VariableOld a))
            pure (env <$ ctx)
        L GetVariableNew -> do
            env <- VariableC (get @(VariableNew a))
            pure (env <$ ctx)
        L (PutVariableOld x) -> do
            env <- VariableC (put @(VariableOld a) x)
            pure (env <$ ctx)
        L (PutVariableNew x) -> do
            env <- VariableC (put @(VariableNew a) x)
            pure (env <$ ctx)
        R other -> VariableC (alg (runVariableC . hdl) (R (R other)) ctx)

runVariable :: forall a m sig b. (Algebra sig m) => a -> VariableC a m b -> m b
runVariable r =
    evalState (VariableNew r) . evalState (VariableOld r) . runVariableC

runInitialConditionM :: forall a m sig b. (Algebra sig m, HasInitialConditionM m a) => VariableC a m b -> m b
runInitialConditionM f = do
    x <- getInitialConditionM @m @a
    runVariable x f