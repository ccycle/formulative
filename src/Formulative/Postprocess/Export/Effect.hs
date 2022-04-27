module Formulative.Postprocess.Export.Effect where

import Control.Algebra
import Formulative.Postprocess.Export.Types

data Export m k where
    AskExportQuantityFormat :: Export m ExportQuantityFormat
    AskEquationType :: Export m EquationType
    AskOutputDir :: Export m OutputDir
    LocalOutputDir :: (OutputDir -> OutputDir) -> m a -> Export m a

askExportQuantityFormat :: (Has Export sig m) => m ExportQuantityFormat
askExportQuantityFormat = send AskExportQuantityFormat
askEquationType :: (Has Export sig m) => m EquationType
askEquationType = send AskEquationType
askOutputDir :: (Has Export sig m) => m OutputDir
askOutputDir = send AskOutputDir
localOutputDir :: (Has Export sig m) => (OutputDir -> OutputDir) -> m a -> m a
localOutputDir f m = send (LocalOutputDir f m)