module Main where

import Control.Carrier.Error.Either
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import OptDEC.Calculation.Internal.Class
import OptDEC.Calculation.Optimization.AugmentedLagrangian
import OptDEC.Calculation.Optimization.LineSearch

main = print "start calculations"

-- 出力パスなどを指定
-- preprocessでの情報を基に計算を開始
-- calculation = runReader (...) $ do
--  x <- get
--  xNew <- calculation x
--  export path xNew

-- 設定ファイルのパスを取得
--