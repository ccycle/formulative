{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Formulative.Preprocess.CommandLineOptions where

import Control.Effect.Error
import Control.Effect.Lift
import Control.Effect.Sum
import Control.Exception.Safe
import Control.Monad
import Data.Hashable
import Dhall hiding (auto)
import Formulative.Preprocess.Exception
import GHC.Generics
import Options.Applicative

-- data RecalculationOption = Overwrite | NoOperation
data RecalculationOption = Continue | Overwrite | NoOperation
    deriving stock (Generic, Show, Eq, Read, Typeable)
    deriving anyclass (FromDhall, ToDhall, Hashable)

data CmdOptions = CmdOptions {filePath :: FilePath, recalculationOption :: RecalculationOption, warningFlag :: Bool}

getLineAndReadM :: (Has (Lift IO) sig m, Member (Throw SomeException) sig, Read a, Typeable a) => m a
getLineAndReadM = do
    str <- sendIO getLine
    liftEither $ readM str

-- すでに計算ファイルが残っている場合は警告を出す
cmdOption :: Parser CmdOptions
cmdOption =
    CmdOptions
        <$> strOption
            ( long "setting-file"
                <> short 's'
                <> metavar "PATH"
                <> help (concat ["Path for Setting file.", "\n", "file format: dhall"])
                <> showDefault
                <> value "./setting.dhall"
            )
        <*> option
            auto
            ( long
                "recalculation"
                <> short 'R'
                <> help
                    ( concat
                        [ "Option for recalculation."
                        , "\n"
                        , "Available options are \'Overwrite\', \'Continue\', \'NoOperation\'."
                        , "\n"
                        , "\'Overwrite\': remove output directory and recalculate."
                        , "\n"
                        , "\'Continue\': recalculate all dependent variables and continue from last step."
                        , "          Useful when you have changed dependent variables or want to calculate from the previous result."
                        , "\n"
                        , "\'NoOperation\': do nothing."
                        ]
                    )
                <> showDefault
                <> value NoOperation
                <> metavar "OPTION"
            )
        <*> switch
            ( long "ignore-warning"
                <> short 'I'
                <> help "Ignore warning. Useful for warning of overwriting."
            )

cmdOptionIO = execParser opts
  where
    opts =
        info
            (cmdOption <**> helper)
            ( fullDesc
                <> progDesc "Execute numerical calculation."
                <> header "Formulative - an open source tool for numerical simulation based on polymorphism in Haskell."
            )
