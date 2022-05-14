module Formulative.Postprocess.Export.Effect where

import Control.Algebra
import Formulative.Postprocess.Export.Types

data Export m k where
    AskLogFilePath :: Export m LogFilePath
    AskOutputDir :: Export m OutputDir
    LocalOutputDir :: (OutputDir -> OutputDir) -> m a -> Export m a

askLogFilePath :: (Has Export sig m) => m LogFilePath
askLogFilePath = send AskLogFilePath
askOutputDir :: (Has Export sig m) => m OutputDir
askOutputDir = send AskOutputDir
localOutputDir :: (Has Export sig m) => (OutputDir -> OutputDir) -> m a -> m a
localOutputDir f m = send (LocalOutputDir f m)
