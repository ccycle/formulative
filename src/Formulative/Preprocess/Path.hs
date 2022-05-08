{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Formulative.Preprocess.Path where

import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import Path
import Path.IO

addTempExtension path = addExtension ".temp" path
addTempExtensions paths = traverse addTempExtension paths

renameTempFile path = do
    flag <- doesFileExist path
    when flag $ do
        path' <- addTempExtension path
        renameFile path path'
renameTempFiles paths = traverse renameTempFile paths

deleteTempFile path = do
    filePath <- addTempExtension path
    flag <- doesFileExist filePath
    when flag $ do
        removeFile filePath
deleteTempFiles paths = mapM_ deleteTempFile paths

renameAndReadTempFiles path = do
    renameTempFile path
    path' <- addTempExtension path
    BSL.readFile (toFilePath path')