{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.DifferentialEquation.Dynamics.Carrier where

import Control.Algebra
import Control.Carrier.Reader
import Control.Effect.Lift
import Control.Effect.Sum
import Control.Effect.Throw
import Control.Exception.Safe
import Dhall
import Formulative.Calculation.DifferentialEquation.Dynamics.Effect
import Formulative.Calculation.DifferentialEquation.Types
import Formulative.Postprocess.Export.Effect
import Formulative.Postprocess.Export.IO
import Formulative.Preprocess.ReadSetting

newtype DynamicsC a m b = DynamicsC {runDynamicsC :: ReaderC (DynamicsSetting a) m b}
    deriving stock (Functor)
    deriving newtype (Applicative, Monad)
instance (Algebra sig m) => Algebra (Dynamics a :+: sig) (DynamicsC a m) where
    alg hdl sig ctx = case sig of
        L AskDynamicsSetting -> do
            env <- DynamicsC ask
            pure ((<$ ctx) env)
        L AskStepSize -> do
            env <- DynamicsC ask
            pure ((<$ ctx) (stepSize env))
        R other -> DynamicsC (alg (runDynamicsC . hdl) (R other) ctx)

runDynamics :: forall a m b. DynamicsSetting a -> DynamicsC a m b -> m b
runDynamics x = runReader x . runDynamicsC

runDynamicsIO ::
    forall a m b sig.
    ( Algebra sig m
    , Member (Throw SomeException) sig
    , ToDhall a
    , FromDhall a
    , Member (Lift IO) sig
    ) =>
    DynamicsC a m b ->
    m b
runDynamicsIO f = do
    (DhallSettingText txt) <- cmdOptionToDhallSettingText
    putStrLnM "Reading setting file (Dynamics).."
    r <- sendIO $ readRecordFromDhallFile @(DynamicsSetting a) "dynamics" txt
    runReader r . runDynamicsC $ f