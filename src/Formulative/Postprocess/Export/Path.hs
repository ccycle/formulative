{-# LANGUAGE TemplateHaskell #-}

module Formulative.Postprocess.Export.Path where

import Control.Algebra
import Control.Effect.Lift
import Control.Effect.Sum
import Control.Monad (when)
import Crypto.Hash hiding (hash)
import Data.ByteString (ByteString)
import Data.Csv (ToField (toField))
import Data.Hashable
import Data.Void
import Formulative.Postprocess.Export.Effect
import Formulative.Postprocess.Export.Types
import Formulative.Preprocess.CommandLineOptions (CmdOptions (CmdOptions, recalculationOption), RecalculationOption (Continue), cmdOptionIO)
import Language.Haskell.TH (Extension (TemplateHaskell))
import Numeric (showHex)
import Path
import Path.IO (doesDirExist, removeDirRecur)
import Replace.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

-- https://hackage.haskell.org/package/replace-megaparsec-1.4.4.0#usage-examples

{- | Parse and replace string.
 @
 >>> parseAndReplace "$(output)" "output/time" "./test/$(output)/data.csv"
 "./test/output/time/data.csv"
 @
-}
parseAndReplace x y = streamEdit (chunk x :: Parsec Void String String) (const y)

hashHexadecimalString x = show iHash
 where
  i = hash x
  iBS = toField i
  iHash = hashWith SHA1 iBS

outputDirCmdStr = "[[output]]"
replaceOutputRelDir :: (Has Export sig m) => String -> m String
replaceOutputRelDir x = do
  (OutputDir dir) <- askOutputDir
  let dirStr = toFilePath dir
  return $ parseAndReplace outputDirCmdStr dirStr x

-- >>> replaceHash (defaultValue @(FormulativeSetting Double)) (OutputDirSetting "./test/[[hash]]/data.csv")
-- OutputDirSetting "./test/6eb63ae0d5b0748c/data.csv"
replaceHash :: (Hashable a) => a -> OutputDirSetting -> OutputDirSetting
replaceHash a (OutputDirSetting x) =
  let hashVal = hashHexadecimalString a
   in OutputDirSetting $ parseAndReplace outputDirHashCmdStr hashVal x

newtype SettingHash = SettingHash String

addPostfixToDirForDependentVariable (OutputDir path) = do
  let p = $(mkRelDir "dependentVariable")
   in OutputDir (path </> p)

removeDependentVariableDirM :: (Algebra sig m, Member Export sig, Member (Lift IO) sig) => m ()
removeDependentVariableDirM = do
  CmdOptions{..} <- sendIO cmdOptionIO
  case recalculationOption of
    Continue -> do
      localOutputDir addPostfixToDirForDependentVariable $ do
        OutputDir path <- askOutputDir
        flag <- sendIO $ doesDirExist path
        when flag $
          sendIO $ removeDirRecur path
    _ -> return ()
