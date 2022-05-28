{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Preprocess.SettingFile.Carrier where

import Control.Algebra
import Control.Carrier.Reader
import Control.Effect.Lift
import Control.Effect.Sum
import Data.Hashable
import Dhall
import Formulative.Postprocess.Export.Path
import Formulative.Preprocess.CommandLineOptions
import Formulative.Preprocess.DefaultValue
import Formulative.Preprocess.ReadSetting
import Formulative.Preprocess.SettingFile.Effect

newtype SettingFileC m a = SettingFileC {runSettingFileC :: ReaderC (DhallSettingText, SettingHash) m a}
    deriving stock (Functor)
    deriving newtype (Applicative, Monad)
instance (Algebra sig m) => Algebra (SettingFile :+: sig) (SettingFileC m) where
    alg hdl sig ctx = case sig of
        L AskSettingFileText -> do
            (env, _) <- SettingFileC (ask @(DhallSettingText, SettingHash))
            pure (env <$ ctx)
        L AskSettingHash -> do
            (_, env) <- SettingFileC (ask @(DhallSettingText, SettingHash))
            pure (env <$ ctx)
        L (LocalSettingFileText f m) ->
            (SettingFileC . ReaderC)
                (\(txt, hashVal) -> run (f txt, hashVal) (hdl (m <$ ctx)))
          where
            run r = runReader r . runSettingFileC
        R other -> SettingFileC (alg (runSettingFileC . hdl) (R other) ctx)

-- Carrier 1: Pure value
runSettingFile :: (Hashable a, ToDhall a) => a -> SettingFileC m b -> m b
runSettingFile s f = do
    let b = hashHexadecimalString s
    let x = toDhallText s
    runReader (DhallSettingText x, SettingHash b) . runSettingFileC $ f

-- Carrier 2: IO
-- runSettingFileIO @MySetting ...
runSettingFileIO ::
    forall a sig m b.
    ( Algebra sig m
    , Member (Lift IO) sig
    , HasDefaultValue a
    , Hashable a
    , FromDhall a
    , ToDhall a
    ) =>
    SettingFileC m b ->
    m b
runSettingFileIO f = do
    CmdOptions{..} <- sendIO cmdOptionIO
    (DhallSettingText x) <- sendIO $ readDhallFile filePath
    -- s <- sendIO $ input @a auto x
    s <- sendIO $ fillInMissingValuesWithDefaultValues @a x
    let x' = toDhallText s
    let b = hashHexadecimalString s
    runReader (DhallSettingText x', SettingHash b) . runSettingFileC $ f
