module Main where

import Control.Carrier.Error.Either
import Control.Carrier.Reader
import Control.Carrier.State.Strict
import HStructure.Calculation.Internal.Class
import HStructure.Calculation.Optimization.AugmentedLagrangian
import HStructure.Calculation.Optimization.LineSearch

main = print "start calculations"

-- 出力パスなどを指定
-- preprocessでの情報を基に計算を開始
-- calculation = runReader (...) $ do
--  x <- get
--  xNew <- calculation x
--  export path xNew

-- 設定ファイルのパスを取得
--