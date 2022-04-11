module Formulative.Preprocess.DiscreteExteriorCalculus.CheckMesh where

-- メッシュのチェック
-- 線の長さが0になってないか
-- 体積が0になっていないか
-- トポロジーがおかしくないか
-- メッシュ番号が0から順番にすべて揃っているか(抜けがないか)
-- k-simplexの番号の列の数がすべて同じか
-- [
--   [0,1,2]
-- , [1,2,3,4]
-- ]
-- とかになってるとだめ