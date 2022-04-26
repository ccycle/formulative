module Main (main) where

import Control.Carrier.Lift
import Control.Exception.Safe
import Dhall
import Formulative.Calculation.Internal.Setting
import Formulative.Postprocess.Export.Class
import Formulative.Preprocess.DefaultValue
import Formulative.Preprocess.Exception
import Formulative.Preprocess.ReadSetting (writeDhallFile)
import Path (parseRelFile)

-- Fieldの場合
--  - 座標データの読み込み
--  - 設定ファイルからの入力値と座標データの長さが一致するかを確認
--  - トポロジーデータの読み込み
--  - トポロジーデータを基にSimplicesを生成
--  - メッシュデータのトポロジーチェック
--  - インデックスのチェック
--  - 座標値のチェック

mainPreprocess = do
    sendIO $ putStrLn "Exporting Default settings.."
    a <- sendIO $ parseRelFile "./default_values.dhall"
    msgExportFileM a
    sendIO $ writeDhallFile "./default_values.dhall" (defaultValue @(FormulativeSetting Double))

main = runM mainPreprocess -- `catch` errorHandlingPreprocess

-- Configを読み取る
-- メッシュなどに異常がないか確認

-- preprocess = runReader (...) $ do
--  checkMesh
--  ...
--