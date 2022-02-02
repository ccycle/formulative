module Main where

import Control.Exception.Safe
import Dhall
import HStructure.Preprocess.Exception

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
    putStrLn "start preprocessing"
    readtest :: Double <- readM "bad input"
    putStrLn $ "readM test: (input value)+1=" ++ show (readtest + 1)
    putStrLn "end"

errorHandlingPreprocess = printError

main = mainPreprocess `catch` errorHandlingPreprocess

-- Configを読み取る
-- メッシュなどに異常がないか確認

-- preprocess = runReader (...) $ do
--  checkMesh
--  ...
--