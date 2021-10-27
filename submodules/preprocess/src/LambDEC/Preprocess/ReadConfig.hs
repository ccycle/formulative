{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module LambDEC.Preprocess.ReadConfig where

import Data.Extensible
import Data.Functor.Identity
import Data.Proxy
import Data.Text
import Dhall
import GHC.TypeLits

-- instance Forall (KeyValue KnownSymbol (Instance1 Interpret h)) xs => Interpret (Field h :* xs) where
--     autoWith opts =
--         Dhall.record
--             ( hgenerateFor
--                 (Proxy :: Proxy (KeyTargetAre KnownSymbol (Instance1 Interpret h)))
--                 ( \m ->
--                     let k = (pack . symbolVal . proxyAssocKey) m
--                      in field k (fmap Field (autoWith opts))
--                 )
--             )

-- deriving instance Interpret (h (AssocValue kv)) => Interpret (Field h kv)
-- deriving instance Interpret a => Interpret (Identity a)
