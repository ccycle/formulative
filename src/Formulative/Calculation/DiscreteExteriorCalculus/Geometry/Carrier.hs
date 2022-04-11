{-# LANGUAGE UndecidableInstances #-}

module Formulative.Calculation.DiscreteExteriorCalculus.Geometry.Carrier where

import Control.Algebra
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Formulative.Calculation.DiscreteExteriorCalculus.Geometry.Effect
import Formulative.Calculation.DiscreteExteriorCalculus.Geometry.Types

-- Carrier 1: fixed point
newtype PointDataC (n :: EucDim) p a m b = PointDataC {runPointDataC :: ReaderC (AllPointDataPrimal0 n p a) m b}
    deriving stock (Functor)
    deriving newtype (Applicative, Monad)
runPointData :: AllPointDataPrimal0 n p a -> PointDataC n p a m b -> m b
runPointData s = runReader s . runPointDataC

instance (Algebra sig m) => Algebra (PointData n p a :+: sig) (PointDataC n p a m) where
    alg hdl sig ctx = case sig of
        L GetPointData -> do
            l <- PointDataC (ask @(AllPointDataPrimal0 n p a))
            pure ((<$ ctx) l)
        R other -> PointDataC (alg (runPointDataC . hdl) (R other) ctx)

-- Carrier 2: moving point
newtype MovingPointDataC (n :: EucDim) p a m b = MovingPointDataC {runMovingPointDataC :: StateC (AllPointDataPrimal0 n p a) m b}
    deriving stock (Functor)
    deriving newtype (Applicative, Monad)
runMovingPointData :: Functor m => AllPointDataPrimal0 n p a -> MovingPointDataC n p a m b -> m b
runMovingPointData s = evalState s . runMovingPointDataC

instance (Algebra sig m) => Algebra (PointData n p a :+: sig) (MovingPointDataC n p a m) where
    alg hdl sig ctx = case sig of
        L GetPointData -> do
            l <- MovingPointDataC (get @(AllPointDataPrimal0 n p a))
            pure ((<$ ctx) l)
        R other -> MovingPointDataC (alg (runMovingPointDataC . hdl) (R other) ctx)
