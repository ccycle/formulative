{-# LANGUAGE AllowAmbiguousTypes #-}

module Formulative.Preprocess.IO where

import Control.Algebra
import Control.Effect.Error
import Control.Effect.Lift
import Control.Effect.Sum
import Control.Exception.Safe
import Dhall
import Formulative.Calculation.Internal.Class
import Formulative.Postprocess.Export.Effect
import Formulative.Postprocess.Export.IO
import Formulative.Postprocess.Export.Path (removeDependentVariableDirM)
import Formulative.Postprocess.Export.SettingFile
import Formulative.Preprocess.SettingFile.Effect

preprocessM ::
    forall m a sig.
    ( Algebra sig m
    , Member (Throw SomeException) sig
    , Member (Lift IO) sig
    , Member Export sig
    , Member SettingFile sig
    , HasDependentParameterM m a
    , ToDhall (DependentParameterType a)
    ) =>
    m ()
preprocessM = do
    removeDirRecurWithWarningM
    removeDependentVariableDirM
    ensureDirOutputM
    exportSettingFile
    exportDependentParameterFile @a @m
