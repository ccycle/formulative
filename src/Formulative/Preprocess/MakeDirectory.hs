-- {-# LANGUAGE DeriveAnyClass #-}

module Formulative.Preprocess.MakeDirectory where

import Control.Exception.Safe
import Data.Hashable
import Dhall hiding (auto)

-- TODO: ファイルの変更を検知できるようにする
-- ソースコードが変更されれば別のファイルになるようにする
-- ハッシュ関数の作成
-- directoryはpackage.yamlのexectablesのsource先ファイルをすべて持ってくる
-- 設定ファイルはレコードで管理、できる限りファイルそのものではなく値のみで判定したい
-- おそらくソースコードに関してはコンパイルして同じ挙動になることを保証できない(コメントだけ足した場合など)
-- ここはファイル変更検知でいいはず

-- data RecalculationRule = Continue | Overwrite | NoOperation
--     deriving stock (Generic, Show, Read)
--     deriving anyclass (FromDhall, ToDhall, Hashable)

-- -- path for setting file,
-- cmdOption :: Parser RecalculationRule
-- cmdOption = option auto (long "RecalculationRule" <> short 'R' <> help "" <> showDefault <> value Continue <> metavar "")