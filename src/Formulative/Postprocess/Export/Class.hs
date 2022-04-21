{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}

module Formulative.Postprocess.Export.Class where

import Control.Algebra
import Control.Carrier.Error.Either
import Control.Effect.Lift
import Control.Effect.Sum
import Control.Exception.Safe
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import Data.Csv hiding (Field)
import qualified Data.Matrix.Static.Generic as MSG
import qualified Data.Matrix.Static.Sparse as MSS
import Data.Proxy
import Data.Singletons (Sing (..), SingI, SingInstance (SingInstance), SingKind (toSing), SomeSing (SomeSing), singInstance)
import Data.Singletons.Prelude (SList)
import qualified Data.Text.IO as T
import Data.Time
import qualified Data.Vector.Storable as VST
import Dhall
import Formulative.Calculation.DiscreteExteriorCalculus.DifferentialForm.Types
import Formulative.Calculation.DiscreteExteriorCalculus.Geometry.Types
import Formulative.Calculation.DiscreteExteriorCalculus.Homology.Types
import Formulative.Calculation.Internal.Class
import Formulative.Calculation.Internal.Singletons (SomeSingI (SomeSingI), someSingVal, withSomeSingI)
import Formulative.Calculation.VectorSpace.VectorSpace (Scalar)
import Formulative.Postprocess.Export.Effect
import Formulative.Postprocess.Export.Types
import Formulative.Preprocess.CommandLineOptions
import Formulative.Preprocess.ReadSetting
import Formulative.Preprocess.SettingFile.Effect (SettingFile, askSettingFileText)
import GHC.TypeNats
import Path
import Path.IO

putStrLnM :: (Algebra sig m, Member (Lift IO) sig) => String -> m ()
putStrLnM = sendIO . putStrLn

-- TODO: (Int,Int,a)の形で出力できるようにする
-- instance (ToField a, Storable a, MSS.Zero a) => ToField (MSL.SparseMatrix m n a) where
--     toField x = toStrict . encode $ (Prelude.map (: []) . MSG.toList $ x)

-- TODO: toStrictを使わない形に直す
-- instance (ToField a, VST.Storable a, MSS.Zero a) => ToField (DECrepresentationMatrix n l c1 k1 c2 k2 a) where
--     toField (DECrepresentationMatrix mat) = BSL.toStrict . encode $ Prelude.map (: []) . MSG.toList $ mat

exportSettingFile ::
    forall m sig.
    ( Algebra sig m
    , Member Export sig
    , Member SettingFile sig
    , Member (Lift IO) sig
    ) =>
    m ()
exportSettingFile = do
    (OutputDir outputDir) <- askOutputDir
    (DhallSettingText x) <- askSettingFileText
    putStrLnM "Export setting file.."
    sendIO $ ensureDir outputDir
    sName <- sendIO $ parseRelFile "setting.dhall"
    sendIO $ T.writeFile (toFilePath (outputDir </> sName)) x
    putStrLnM "Done."

exportDependentParameterFile ::
    forall a m sig.
    ( Algebra sig m
    , Member Export sig
    , Member (Lift IO) sig
    , HasDependentParameterM m a
    , ToDhall (DependentParameterType a)
    ) =>
    m ()
exportDependentParameterFile = do
    x <- dependentParameterM @m @a
    let y = toDhallText x
    when (y /= toDhallText ()) $ do
        (OutputDir outputDir) <- askOutputDir
        putStrLnM "Export dependent parameter file.."
        sendIO $ ensureDir outputDir
        sName <- sendIO $ parseRelFile "dependentParamater.dhall"
        sendIO $ T.writeFile (toFilePath (outputDir </> sName)) y
        putStrLnM "Done."

-- 1ファイルに追記
exportDependentVariableGlobal ::
    forall a sig m.
    ( Algebra sig m
    , Member (Lift IO) sig
    , Member (Throw SomeException) sig
    , Member Export sig
    , HasDependentVariableGlobalM m a
    , DefaultOrdered (DependentVariableGlobalType a)
    , ToNamedRecord (DependentVariableGlobalType a)
    , ToRecord (DependentVariableGlobalType a)
    ) =>
    a ->
    m ()
exportDependentVariableGlobal x = do
    x' <- dependentVariableGlobalM x
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
            let str = encodeDefaultOrderedByName [x']
            sendIO $ BSL.writeFile (toFilePath filePath) str

ensureDirOutputM :: (Algebra sig m, Member Export sig, Member (Lift IO) sig) => m ()
ensureDirOutputM = do
    (OutputDir relDir) <- askOutputDir
    sendIO . ensureDir $ relDir

doesDirExistsOutputM :: (Algebra sig m, Member Export sig, Member (Lift IO) sig) => m Bool
doesDirExistsOutputM = do
    (OutputDir relDir) <- askOutputDir
    sendIO . doesDirExist $ relDir

removeDirRecurOutputM :: (Algebra sig m, Member (Lift IO) sig) => OutputDir -> m ()
removeDirRecurOutputM (OutputDir relDir) =
    sendIO . removeDirRecur $ relDir

-- TODO: logger作成
warningForOverwrite :: (Has (Lift IO) sig m, Member (Throw SomeException) sig) => OutputDir -> m RecalculationOption
warningForOverwrite (OutputDir dir) = do
    putStrLnM $ "[WARNING] The output directory (" <> toFilePath dir <> ") already exists: The calculation may have already been executed."
    putStrLnM ""
    putStrLnM "Overwrite ([y]/n)?"
    f 5
  where
    f i =
        if i == 0
            then liftEither $ throwString "Invalid input: stop process."
            else do
                str <- sendIO getLine
                case str of
                    "" -> return Overwrite
                    "y" -> return Overwrite
                    "n" -> return NoOperation
                    _ -> do
                        putStrLnM ""
                        putStrLnM $ "Invalid input: " <> str
                        putStrLnM "RETURN -> Exec and Overwrite"
                        putStrLnM "\'y\', RETURN -> Exec and Overwrite"
                        putStrLnM "\'n\', RETURN -> Exit"
                        f (pred i)

removeDirRecurWithWarningM :: (Algebra sig m, Member (Lift IO) sig, Member Export sig, Member (Throw SomeException) sig) => m ()
removeDirRecurWithWarningM = do
    (OutputDir x) <- askOutputDir
    d <- sendIO $ doesDirExist x
    when d $ do
        r <- warningForOverwrite (OutputDir x)
        case r of
            Overwrite -> removeDirRecurOutputM (OutputDir x)
            NoOperation -> liftEither $ throwString "Exit."

parentDirM :: (Algebra sig m, Member (Lift IO) sig, Member (Throw SomeException) sig) => m (Path Rel Dir)
parentDirM = do
    t <- sendIO getZonedTime
    let dir = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" t
    liftEither $ parseRelDir $ "./output/" <> dir

-- TODO: adaptive step sizeの実装

msgNewLine :: (Has (Lift IO) sig m) => m ()
msgNewLine = putStrLnM ""

msgStart :: (Has (Lift IO) sig m) => m ()
msgStart = putStrLnM "Start."

msgDone :: (Has (Lift IO) sig m) => m ()
msgDone = putStrLnM "Done."

msgEnd :: (Has (Lift IO) sig m) => m ()
msgEnd = putStrLnM "End."

msgOutputDir :: (Member Export sig, Member (Lift IO) sig, Algebra sig m) => m ()
msgOutputDir = do
    (OutputDir outputPath) <- askOutputDir
    absOutputDir <- sendIO $ makeAbsolute outputPath
    putStrLnM $ "output directory: " <> toFilePath absOutputDir

-- class MainCalcPDEManifold m (s :: Nat -> [Nat] -> *) where
--     mainCalcPDEManifold :: (KnownNat n, SingI l) => s n l -> m ()
--     initialStatePDEManifold :: (KnownNat n, SingI l) => m (s n l)

-- class MainCalcPDESubmanifold m (s :: EucDim -> Dim -> [Nat] -> *) where
--     mainCalcPDESubmanifold :: (KnownNat nEuc, KnownNat n, SingI l) => s nEuc n l -> m ()
--     initialStatePDESubmanifold :: (KnownNat nEuc, KnownNat n, SingI (l :: [Nat])) => m (s nEuc n l)

-- mainCalcPDEproxy ::
--     forall s nEuc n (l :: [Nat]) sig m.
--     ( MainCalcPDESubmanifold m s
--     , KnownNat nEuc
--     , KnownNat n
--     , SingI l
--     , Monad m
--     ) =>
--     Proxy nEuc ->
--     Proxy n ->
--     SList l ->
--     m ()
-- mainCalcPDEproxy _ _ _ = do
--     x <- (initialStatePDESubmanifold @m @s @nEuc @n @l)
--     mainCalcPDESubmanifold @m @s @nEuc @n @l x

-- runMainCalcPDE
--     (DimensionOfEuclideanSpace nEuc)
--     (DimensionOfManifold n)
--     (l :: [Natural]) =
--         case (someNatVal nEuc, someNatVal n, someSingVal l) of
--             (SomeNat (pnEuc :: Proxy nEuc'), SomeNat (pn :: Proxy n'), SomeSingI (sl :: Sing l')) ->
--                 mainCalcPDEproxy pnEuc pn sl

preprocessM ::
    forall m a sig.
    ( Algebra sig m
    , Member (Throw SomeException) sig
    , Member (Lift IO) sig
    , Member Export sig
    , Member SettingFile sig
    , HasDependentParameterM m a
    , ToDhall (DependentParameterType a)
    ) =>
    m ()
preprocessM = do
    removeDirRecurWithWarningM
    ensureDirOutputM
    exportSettingFile
    exportDependentParameterFile @a @m