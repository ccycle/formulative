{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Preprocess.ReadSetting where

import Control.Algebra
import Control.Effect.Error
import Control.Effect.Lift
import Control.Effect.Sum
import Control.Exception.Safe
import Data.Hashable (Hashable (hash))
import Data.Text
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Dhall
import Dhall.Pretty
import Formulative.Calculation.Algebra.Arithmetic.Additive (zero)
import Formulative.Calculation.VectorSpace.VectorSpace (Scalar)
import Formulative.Postprocess.Export.Path (hashHexadecimalString)
import Formulative.Preprocess.CommandLineOptions
import Formulative.Preprocess.DefaultValue
import Path

toDhallText x = pack $ show doc
 where
  expression = Dhall.embed Dhall.inject x
  doc = Dhall.Pretty.prettyCharacterSet Unicode expression

-- usage: toDhallTypeText @(DynamicsSetting Double)
toDhallTypeText :: forall a. (ToDhall a) => Text
toDhallTypeText = pack $ show doc
 where
  expression = Dhall.declared (Dhall.inject @a)
  doc = Dhall.Pretty.prettyCharacterSet Unicode expression

writeDhallFile path x = T.writeFile path (toDhallText x)
readDhallFile path = do
  x <- T.readFile path
  return $ DhallSettingText x

readRecordFromDhallFile :: forall a. FromDhall a => Text -> Text -> IO a
readRecordFromDhallFile name txt = input auto $ T.concat ["(", txt, ")", ".", name]

-- example: "{equation.b = 1.0 , dynamics.stepSize = 0.1}""
newtype DhallSettingText = DhallSettingText Text
  deriving (Generic, Show)

fillInMissingValues :: ToDhall a => Text -> a -> Text
fillInMissingValues txt x = T.concat [toDhallText x, " // ", txt]

cmdOptionToDhallSettingText :: (Algebra sig m, Member (Lift IO) sig, Member (Throw SomeException) sig) => m DhallSettingText
cmdOptionToDhallSettingText = do
  CmdOptions{..} <- sendIO cmdOptionIO
  pathRelFile <- liftEither $ parseRelFile filePath
  sendIO $ readDhallFile filePath

fillInMissingValuesWithDefaultValues :: forall a. (FromDhall a, ToDhall a, HasDefaultValue a) => Text -> IO a
fillInMissingValuesWithDefaultValues txt = input auto $ T.concat [toDhallText (defaultValue @a), " // ", txt]

-- input txt to data record
-- no input :: "{=}""
-- fillInSetting "export" txt :: IO (ExportSetting)
fillInSetting :: forall a. (ToDhall a, FromDhall a, HasDefaultValue a) => Text -> Text -> IO a
fillInSetting name txt = input auto $ T.concat ["(", "{", name, " = ", toDhallText (defaultValue @a), "}", " // ", txt, ")", ".", name]
