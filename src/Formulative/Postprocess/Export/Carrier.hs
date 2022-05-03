{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Formulative.Postprocess.Export.Carrier where

import Control.Algebra
import Control.Carrier.Reader
import Control.Effect.Error
import Control.Effect.Lift
import Control.Effect.Sum
import Control.Exception.Safe
import Formulative.Internal.ReExport.Effect (askSettingHash)
import Formulative.Postprocess.Export.Effect
import Formulative.Postprocess.Export.IO
import Formulative.Postprocess.Export.Path
import Formulative.Postprocess.Export.Types
import Formulative.Preprocess.DefaultValue (HasDefaultValue)
import Formulative.Preprocess.ReadSetting
import Formulative.Preprocess.SettingFile.Effect
import Path

newtype ExportC m a = ExportC {runExportC :: ReaderC (ExportQuantityFormat, OutputDir) m a}
    deriving stock (Functor)
    deriving newtype (Applicative, Monad)
instance (Algebra sig m) => Algebra (Export :+: sig) (ExportC m) where
    alg hdl sig ctx = case sig of
        L AskExportQuantityFormat -> do
            (env, _) <- ExportC (ask @(ExportQuantityFormat, OutputDir))
            pure (env <$ ctx)
        L AskOutputDir -> do
            (_, output) <- ExportC (ask @(ExportQuantityFormat, OutputDir))
            pure (output <$ ctx)
        L (LocalOutputDir f m) -> do
            (env, output) <- ExportC ask
            y <- hdl (m <$ ctx)
            run (env, f output) (pure y)
          where
            run r = runReader r . runExportC
        R other -> ExportC (alg (runExportC . hdl) (R other) ctx)

-- runExport ODE @MySetting ...
runExport ::
    forall a m b sig.
    ( Algebra sig m
    , Member (Throw SomeException) sig
    , Member (Lift IO) sig
    , Member SettingFile sig
    ) =>
    ExportSetting ->
    ExportC m b ->
    m b
runExport x f = do
    SettingHash hashStr <- askSettingHash
    let (ExportSetting r (OutputDirSetting str)) = x
    let z = parseAndReplace outputDirHashCmdStr hashStr str
    outputDir <- liftEither $ parseRelDir z
    runReader (r, OutputDir outputDir) . runExportC $ f

runExportIO ::
    forall a m b sig.
    ( Algebra sig m
    , Member (Lift IO) sig
    , Member (Throw SomeException) sig
    , Member SettingFile sig
    ) =>
    ExportC m b ->
    m b
runExportIO f = do
    (DhallSettingText txt) <- cmdOptionToDhallSettingText
    putStrLnM "Reading setting file (Export).."
    x <- sendIO $ fillInSetting @ExportSetting "export" txt
    runExport x f