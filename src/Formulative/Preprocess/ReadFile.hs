module Formulative.Preprocess.ReadFile where

import Control.Applicative
import Control.Exception.Safe
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import Path
import Path.IO

-- 過去の計算結果をパースする用の関数
-- 後で計算したくなったときに使う
-- 例: 質量密度の積分をしていなかったが、あとでしたくなったとき
-- 過去の計算結果のパスを取得、中身をパース、再計算

(</>.) :: MonadThrow m => m (Path b Dir) -> m (Path Rel t) -> m (Path b t)
(</>.) = liftA2 (</>)

inputPathfromName :: MonadThrow f => String -> f FilePath
inputPathfromName fileNamePath = toFilePath <$> parseRelFile fileNamePath

filePathToByteString pathName = BSL.readFile =<< inputPathfromName pathName