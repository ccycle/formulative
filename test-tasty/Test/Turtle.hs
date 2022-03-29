{-# LANGUAGE OverloadedStrings #-}

module Test.Turtle where

import qualified Data.Text as Text
import Turtle

exec :: (MonadIO m) => Text -> m ()
exec cmd = do
    x <- shell cmd empty
    case x of
        ExitSuccess -> return ()
        ExitFailure n -> die ("the command \"" <> cmd <> "\"" <> " failed with exit code: " <> repr n) >> return ()