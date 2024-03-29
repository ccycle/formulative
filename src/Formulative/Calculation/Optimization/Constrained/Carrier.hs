{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.Optimization.Constrained.Carrier where

import Control.Algebra
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Effect.Error
import Control.Effect.Lift
import Control.Effect.Sum
import Control.Exception.Safe
import Data.String.Conversions
import Dhall
import Formulative.Calculation.Algebra.Arithmetic.Additive
import Formulative.Calculation.Internal.Class
import Formulative.Calculation.Internal.Variable.Carrier
import Formulative.Calculation.Optimization.Constrained.Effect
import Formulative.Calculation.Optimization.Constrained.Types
import Formulative.Calculation.VectorSpace.Class
import Formulative.Preprocess.ReadSetting
import Formulative.Preprocess.SettingFile.Effect

-- Carrier
newtype ConstrainedSystemC a m b = ConstrainedSystemC {runConstrainedSystemC :: StateC (LagrangeMultiplier a) (ReaderC (ConstrainedSystemSetting (Scalar a)) m) b}
    deriving stock (Functor)
    deriving newtype (Applicative, Monad)
instance (Algebra sig m) => Algebra (ConstrainedSystem a :+: sig) (ConstrainedSystemC a m) where
    alg hdl sig ctx = case sig of
        L AskConstrainedSystemParameter -> do
            env <- ConstrainedSystemC ask
            pure (env <$ ctx)
        L GetLagrangeMultiplier -> do
            env <- ConstrainedSystemC get
            pure (env <$ ctx)
        L (PutLagrangeMultiplier x) -> do
            env <- ConstrainedSystemC (put x)
            pure (env <$ ctx)
        R other -> ConstrainedSystemC (alg (runConstrainedSystemC . hdl) (R (R other)) ctx)

runConstrainedSystem r =
    runReader r . evalState (LagrangeMultiplier zero) . runConstrainedSystemC

runConstrainedSystemIO ::
    forall a m b sig.
    ( Algebra sig m
    , Member (Throw SomeException) sig
    , Member SettingFile sig
    , Member (Lift IO) sig
    , Additive a
    , ToDhall (Scalar a)
    , FromDhall (Scalar a)
    , Fractional (Scalar a)
    ) =>
    ConstrainedSystemC a m b ->
    m b
runConstrainedSystemIO f = do
    (DhallSettingText txt) <- cmdOptionToDhallSettingText
    r <- sendIO $ fillInSetting @(ConstrainedSystemSetting (Scalar a)) "constrainedSystem" (convertString txt)
    runConstrainedSystem r f

runVariableConstrainedSystem r f = do
    VariablesConstrainedSystem x l <- getInitialConditionM
    runVariable x . runReader r . evalState (LagrangeMultiplier l) . runConstrainedSystemC $ f
