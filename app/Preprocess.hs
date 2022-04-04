module Main (main) where

import Control.Carrier.Lift
import Control.Exception.Safe
import Dhall
import OptDEC.Calculation.Internal.Setting
import OptDEC.Preprocess.DefaultValue
import OptDEC.Preprocess.Exception
import OptDEC.Preprocess.ReadSetting (writeDhallFile)

-- TODO: ファイルパスの解析(PathIOを使う)
-- TODO: コマンドライン引数から設定ファイルのパスを指定できるようにする
-- TODO: 入力ファイルの不正値チェック
-- dimensionOfField n | n == 0 = particle
--                    | otherwise = Field
-- Fieldの場合
--  - 座標データの読み込み
--  - 設定ファイルからの入力値と座標データの長さが一致するかを確認
--  - トポロジーデータの読み込み
--  - トポロジーデータを基にSimplicesを生成
--  - メッシュデータのトポロジーチェック
--  - インデックスのチェック
--  - 座標値のチェック

mainPreprocess = do
    sendIO $ putStrLn "Start preprocessing."
    sendIO $ putStrLn "Export Default settings.."
    sendIO $ writeDhallFile "./default_values.dhall" (defaultValue @(OptDECsetting Double))
    sendIO $ putStrLn "Done."
    sendIO $ putStrLn ""
    sendIO $ putStrLn "End."

main = runM mainPreprocess -- `catch` errorHandlingPreprocess

-- Configを読み取る
-- メッシュなどに異常がないか確認

-- preprocess = runReader (...) $ do
--  checkMesh
--  ...
--