{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.DefineNewEffect where

import Control.Algebra
import Control.Carrier.Reader
import Control.Monad.IO.Class
import Data.Kind
import Data.Proxy
import GHC.TypeNats

-- import Formulative.Calculation.DiscreteExteriorCalculus.Class
-- import Formulative.Calculation.DiscreteExteriorCalculus.Homology

data Teletype (m :: Type -> Type) k where
  Read :: Teletype m String
  Write :: String -> Teletype m ()

read :: Has Teletype sig m => m String
read = send Read

write :: Has Teletype sig m => String -> m ()
write s = send (Write s)

newtype TeletypeIOC m a = TeletypeIOC {runTeletypeIO :: m a}
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (Teletype :+: sig) (TeletypeIOC m) where
  alg hdl sig ctx = case sig of
    L Read -> (<$ ctx) <$> liftIO getLine
    L (Write s) -> ctx <$ liftIO (putStrLn s)
    R other -> TeletypeIOC (alg (runTeletypeIO . hdl) other ctx)

-- semantics

{- | A named piece of data you wish to record a specific 'Metric' for.
 See https://docs.datadoghq.com/guides/dogstatsd/ for more details.
-}
data Stat = Stat
  { -- | Stat name, usually separated by '.' (e.g. "system.metric.name")
    statName :: String
  , -- | 'Metric' value.
    statValue :: Metric
  , -- | Key/value 'Tags' (optional).
    statTags :: Tags
  }

-- | The various supported metric types in Datadog.
data Metric
  = -- | Counters track how many times something happens per second.
    Counter Int
  | -- | Gauges track the ebb and flow of a particular metric value over time.
    Gauge Double
  | -- | Histograms calculate the statistical distribution of any kind of value.
    Histogram Double
  | -- | Sets count the number of unique elements in a group
    Set Double
  | -- | Timers measure the amount of time a section of code takes to execute.
    Timer Double

-- | Tags are key/value annotations. Values can blank.
type Tags = [(String, String)]

-- | Logging level
data Level
  = Error
  | Warning
  | Info
  | Debug
  deriving (Eq, Ord, Show)

data Telemetry (m :: * -> *) k where
  WriteStat :: Stat -> Telemetry m ()
  WriteLog :: Level -> String -> [(String, String)] -> Telemetry m ()

data LogQueue
data StatQueue

-- | Run a 'Telemetry' effect by expecting a 'Reader' of 'Queue's to write stats and logs to.
runTelemetry :: LogQueue -> StatQueue -> TelemetryC m a -> m a
runTelemetry logger statter = runReader (logger, statter) . runTelemetryC

newtype TelemetryC m a = TelemetryC {runTelemetryC :: ReaderC (LogQueue, StatQueue) m a}
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

instance (Algebra sig m, MonadIO m) => Algebra (Telemetry :+: sig) (TelemetryC m) where
  alg hdl sig ctx = case sig of
    L op -> do
      queues <- TelemetryC (ask @(LogQueue, StatQueue))
      case op of
        WriteStat stat -> ctx <$ return () -- queueStat (snd queues) stat
        WriteLog level message pairs -> ctx <$ return () -- queueLogMessage (fst queues) level message pairs
    R other -> TelemetryC (alg (runTelemetryC . hdl) (R other) ctx)

-- dependent type
-- data Connectivity (n :: Nat) (l :: SSizes) (m :: Type -> Type) k where
--   GetPrimitiveSimplicies :: Connectivity n l m (Simplices n l)
-- getPrimitiveSimplicies :: (Has (Connectivity n l) sig m) => m (Simplices n l)
-- getPrimitiveSimplicies = send GetPrimitiveSimplicies

-- -- AllowAmbiguousTypesを有効化する必要あり
-- getKSimplicialComplexEff :: forall k n l sig m. (KnownNat k, KnownNat n, Has (Connectivity n l) sig m) => Proxy k -> m (Simplices k l)
-- getKSimplicialComplexEff _ = do
--   generateSimplicialKComplexFromN (Proxy :: Proxy k) <$> getPrimitiveSimplicies @n