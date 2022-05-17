module Formulative.Postprocess.Export.Variable.Global where

import Control.Algebra
import Control.Carrier.Lift
import Control.Effect.Error
import Control.Effect.Sum
import Control.Exception.Safe
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as BSL
import Data.Csv (DefaultOrdered (..), ToRecord)
import Formulative.Calculation.Internal.Class
import Formulative.Postprocess.Export.CSV (encodeLF)
import Formulative.Postprocess.Export.Effect
import Formulative.Postprocess.Export.IO
import Formulative.Postprocess.Export.Path
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
    unless (null (headerOrder x')) $ do
        (OutputDir parentDir) <- askOutputDir
        let (OutputDir dir) = addPostfixToDirForDependentVariable (OutputDir parentDir)
        sendIO $ ensureDir dir
        parseKey <- liftEither $ parseRelFile "_global"
        fileName <- liftEither $ replaceExtension ".csv" parseKey
        let filePath = dir </> fileName
        flag <- sendIO $ doesFileExist filePath
        msgLoggerM $ concat ["Exporting ", toFilePath filePath, " .."]
        if flag
            then do
                -- fileがすでに存在したときは末尾に付け足す
                let str = encodeLF [x']
                sendIO $ BSL.appendFile (toFilePath filePath) str
            else do
                -- 存在しないときは新規に作成
                let str = encodeLF [headerOrder x']
                sendIO $ BSL.writeFile (toFilePath filePath) str
                let str1 = encodeLF [x']
                sendIO $ BSL.appendFile (toFilePath filePath) str1
