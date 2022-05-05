module Formulative.Postprocess.Export.Variable.Global where

import Control.Algebra
import Control.Carrier.Lift
import Control.Effect.Error
import Control.Effect.Sum
import Control.Exception.Safe
import qualified Data.ByteString.Lazy as BSL
import Data.Csv (DefaultOrdered (..), ToRecord, encode)
import Formulative.Calculation.Internal.Class
import Formulative.Postprocess.Export.Effect
import Formulative.Postprocess.Export.IO
import Formulative.Postprocess.Export.Types
import Path
import Path.IO

exportGlobalDependentVariable ::
    forall a sig m.
    ( Algebra sig m
    , Member (Lift IO) sig
    , Member (Throw SomeException) sig
    , Member Export sig
    , HasGlobalDependentVariableM m a
    , DefaultOrdered (GlobalDependentVariable a)
    , ToRecord (GlobalDependentVariable a)
    ) =>
    a ->
    m ()
exportGlobalDependentVariable x = do
    x' <- globalDependentVariableM x
    (OutputDir parentDir) <- askOutputDir
    ensureDirOutputM
    parseKey <- liftEither $ parseRelFile "dependentVariableGlobal"
    -- TODO: CSV以外にもに対応させる
    fileName <- liftEither $ replaceExtension ".csv" parseKey
    let filePath = parentDir </> fileName
    flag <- sendIO $ doesFileExist filePath
    if flag
        then do
            -- fileがすでに存在したときは末尾に付け足す
            let str = encode [x']
            sendIO $ BSL.appendFile (toFilePath filePath) str
        else do
            -- 存在しないときは新規に作成
            let str = encode [headerOrder x']
            sendIO $ BSL.writeFile (toFilePath filePath) str
            let str1 = encode [x']
            sendIO $ BSL.appendFile (toFilePath filePath) str1
