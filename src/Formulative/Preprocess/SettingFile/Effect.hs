module Formulative.Preprocess.SettingFile.Effect where

import Control.Algebra
import Data.Text
import Formulative.Postprocess.Export.Path
import Formulative.Preprocess.ReadSetting

----------------------------------------------------------------
-- read setting File
----------------------------------------------------------------
data SettingFile m k where
    AskSettingHash :: SettingFile m SettingHash -- "./setting.dhall"
    AskSettingFileText :: SettingFile m DhallSettingText -- text in "./setting.dhall"
    LocalSettingFileText :: (DhallSettingText -> DhallSettingText) -> m a -> SettingFile m a

askSettingFileText :: (Has SettingFile sig m) => m DhallSettingText
askSettingFileText = send AskSettingFileText
askSettingHash :: (Has SettingFile sig m) => m SettingHash
askSettingHash = send AskSettingHash
localSettingFileText f m = send (LocalSettingFileText f m)