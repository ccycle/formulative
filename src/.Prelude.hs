module Prelude
  (
    -- * Custom @Prelude@

    -- | One of the core features of @rio@ is that it can be used as a @Prelude@
    -- replacement. Therefore it is best to disable the default `Prelude` with:
    -- [NoImplicitPrelude](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-NoImplicitPrelude)
    -- pragma:
    --
    -- > {-# LANGUAGE NoImplicitPrelude #-}
    -- > import RIO
    --
    -- Some functions not exported here can be found in "RIO.Partial":
    -- @fromJust@, @read@, @toEnum@, @pred@, @succ@.
    --
    module RIO.Prelude
  , module RIO.Prelude.Types
    -- ** @SimpleApp@
    -- | If all you need is just some default environment that does basic logging and allows
    -- spawning processes, then you can use `SimpleApp`:
    --
    -- > {-# LANGUAGE OverloadedStrings #-}
    -- > module Main where
    -- >
    -- > main :: IO ()
    -- > main =
    -- >   runSimpleApp $ do
    -- >     logInfo "Hello World!"
    --
    -- Note the
    -- [OverloadedStrings](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#extension-OverloadedStrings)
    -- extension, which is enabled to simplify logging.
  , module RIO.Prelude.Simple
    -- ** Deque
  , module RIO.Deque
  , module RIO.List

  ) where

import RIO.Deque
import RIO.Prelude
import RIO.Prelude.Simple
import RIO.Prelude.Types
import RIO.List

--------------------------------------------------------------------------------
-- $logging-intro
--
-- The logging system in RIO is built upon "log functions", which are
-- accessed in RIO's environment via a class like "has log
-- function". There are two provided:
--
-- * In the common case: for logging plain text (via 'Utf8Builder')
--   efficiently, there is 'LogFunc', which can be created via
--   'withLogFunc', and is accessed via 'HasLogFunc'. This provides
--   all the classical logging facilities: timestamped text output
--   with log levels and colors (if terminal-supported) to the
--   terminal. We log output via 'logInfo', 'logDebug', etc.
--
-- * In the advanced case: where logging takes on a more semantic
--   meaning and the logs need to be digested, acted upon, translated
--   or serialized upstream (to e.g. a JSON logging server), we have
--   'GLogFunc' (as in "generic log function"), and is accessed via
--   'HasGLogFunc'. In this case, we log output via 'glog'. See the
--   Type-generic logger section for more information.