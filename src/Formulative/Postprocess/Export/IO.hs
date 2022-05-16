{-# LANGUAGE TemplateHaskell #-}

module Formulative.Postprocess.Export.IO where

import Control.Algebra
import Control.Carrier.Error.Church
import Control.Effect.Lift
import Control.Effect.Sum
import Control.Exception.Safe
import Control.Monad
import Data.Time
import Formulative.Postprocess.Export.Effect
import Formulative.Postprocess.Export.Types
import Formulative.Preprocess.CommandLineOptions
import Formulative.Preprocess.SettingFile.Effect
import Path
import Path.IO

msgLoggerM :: (Algebra sig m, Member (Lift IO) sig, Member Export sig) => String -> m ()
msgLoggerM msg = do
    sendIO $ putStrLn msg
    LogFilePath dir <- askLogFilePath
    sendIO . ensureDir $ parent dir
    sendIO $ appendFile (toFilePath dir) (msg <> "\n")

msgM :: (Algebra sig m, Member (Lift IO) sig) => String -> m ()
msgM = sendIO . putStrLn

ensureDirOutputM :: (Algebra sig m, Member Export sig, Member (Lift IO) sig) => m ()
ensureDirOutputM = do
    (OutputDir relDir) <- askOutputDir
    sendIO . ensureDir $ relDir

doesDirExistsOutputM :: (Algebra sig m, Member Export sig, Member (Lift IO) sig) => m Bool
doesDirExistsOutputM = do
    (OutputDir relDir) <- askOutputDir
    sendIO . doesDirExist $ relDir

removeDirRecurOutputM :: (Algebra sig m, Member (Lift IO) sig) => OutputDir -> m ()
removeDirRecurOutputM (OutputDir relDir) =
    sendIO . removeDirRecur $ relDir

msgDirAlreadyExists :: (Monad m, Member (Lift IO) sig, Member (Lift IO) sig, Algebra sig m, Member Export sig) => m ()
msgDirAlreadyExists = do
    (OutputDir dir) <- askOutputDir
    msgLoggerM $ "[WARNING] The output directory (" <> toFilePath dir <> ") already exists; the calculation may have been executed."

data NoOperationException = NoOperationException
    deriving stock (Show, Typeable)

instance Exception NoOperationException where
    displayException _ =
        concat
            [ "*** NoOperationException: Exit process."
            , "\n"
            , "The current option for recalculation is \'NoOperation\'. To recalculate, use option \'--recalculation\'."
            , "\n"
            , "\n"
            , "Available options for \'--recalculation\': \'NoOperation\',\'Overwrite\',\'Continue\'"
            , "\n"
            ]

-- TODO: logger作成
warningForOverwrite :: (Has (Lift IO) sig m, Member (Throw SomeException) sig, Member Export sig) => m RecalculationOption
warningForOverwrite = do
    msgDirAlreadyExists
    CmdOptions{..} <- sendIO cmdOptionIO
    case recalculationOption of
        NoOperation -> return NoOperation
        Continue -> return Continue
        Overwrite ->
            if warningFlag
                then return Overwrite
                else do
                    msgNewLine
                    msgLoggerM $ "Overwrite ([y]/n)?"
                    f 5
  where
    f i =
        if i == 0
            then liftEither $ throwM NoOperationException
            else do
                str <- sendIO getLine
                case str of
                    "" -> return Overwrite
                    "y" -> return Overwrite
                    "n" -> return NoOperation
                    _ -> do
                        msgLoggerM $ "Invalid input: " <> str
                        msgLoggerM "RETURN -> Exec and Overwrite"
                        msgLoggerM "\'y\', RETURN -> Exec and Overwrite"
                        msgLoggerM "\'n\', RETURN -> Exit"
                        f (pred i)

removeDirRecurWithWarningM :: (Algebra sig m, Member (Lift IO) sig, Member Export sig, Member (Throw SomeException) sig) => m ()
removeDirRecurWithWarningM = do
    OutputDir x <- askOutputDir
    sName <- sendIO $ parseRelFile "setting.dhall"
    d <- sendIO $ doesFileExist (x </> sName)
    when d $ do
        r <- warningForOverwrite
        case r of
            Overwrite -> removeDirRecurOutputM (OutputDir x)
            NoOperation -> liftEither $ throw NoOperationException
            _ -> return ()

parentDirM :: (Algebra sig m, Member (Lift IO) sig, Member (Throw SomeException) sig) => m (Path Rel Dir)
parentDirM = do
    t <- sendIO getZonedTime
    let dir = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" t
    liftEither $ parseRelDir $ "./output/" <> dir

-- TODO: adaptive step sizeの実装

msgNewLine :: (Has (Lift IO) sig m, Member Export sig) => m ()
msgNewLine = msgLoggerM ""

msgStart :: (Has (Lift IO) sig m, Member Export sig) => m ()
msgStart = msgLoggerM "--- Start ---"

msgDone :: (Has (Lift IO) sig m, Member Export sig) => m ()
msgDone = msgLoggerM "Done."

msgEnd :: (Has (Lift IO) sig m, Member Export sig) => m ()
msgEnd = msgLoggerM "--- End ---"

msgExportFileIO :: Path b File -> IO ()
msgExportFileIO path = putStrLn $ concat ["Exporting ", toFilePath path, " .."]

msgExportFileM :: (Has (Lift IO) sig m) => Path b File -> m ()
msgExportFileM path = sendIO $ msgExportFileIO path

msgOutputDir :: (Member Export sig, Member (Lift IO) sig, Algebra sig m) => m ()
msgOutputDir = do
    (OutputDir outputPath) <- askOutputDir
    msgLoggerM $ "Output directory: " <> toFilePath outputPath

-- askOutputDirAbsPath :: (Member Export sig, Algebra sig m, Member (Lift IO) sig) => m (Path Abs Dir)
-- askOutputDirAbsPath = do
--     (OutputDir outputPath) <- askOutputDir
--     sendIO $ makeAbsolute outputPath
