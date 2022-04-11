{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.Optimization.Carrier where

import Control.Algebra
import Control.Carrier.Reader
import Control.Effect.Error
import Control.Effect.Lift
import Control.Effect.Sum
import Control.Exception.Safe
import Data.String.Conversions
import Dhall
import Formulative.Calculation.Optimization.Effect
import Formulative.Calculation.Optimization.Parameter
import Formulative.Preprocess.ReadSetting
import Formulative.Preprocess.SettingFile.Effect

-- Carrier 1: Reader
newtype OptimizationC a m b = OptimizationC {runOptimizationC :: ReaderC (OptimizationParameters a) m b}
    deriving stock (Functor)
    deriving newtype (Applicative, Monad)
instance (Algebra sig m) => Algebra (Optimization a :+: sig) (OptimizationC a m) where
    alg hdl sig ctx = case sig of
        L AskOptimizationParameters -> do
            env <- OptimizationC ask
            pure (env <$ ctx)
        R other -> OptimizationC (alg (runOptimizationC . hdl) (R other) ctx)

runOptimization :: forall a m b. OptimizationParameters a -> OptimizationC a m b -> m b
runOptimization r = runReader r . runOptimizationC

-- Carrier 2: IO
-- ./setting.dhallから読み込む
runOptimizationIO ::
    forall a m b sig.
    ( Algebra sig m
    , Member SettingFile sig
    , Member (Throw SomeException) sig
    , Member (Lift IO) sig
    , ToDhall a
    , FromDhall a
    , Fractional a
    ) =>
    OptimizationC a m b ->
    m b
runOptimizationIO f = do
    (DhallSettingText txt) <- cmdOptionToDhallSettingText
    r <- sendIO $ fillInSetting @(OptimizationParameters a) "optimization" txt
    runOptimization r f